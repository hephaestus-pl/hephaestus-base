module ExportProduct (exportProduct, exportUcmToLatex, exportAspectInterfacesToLatex, exportSourceCode) where

import System.IO
-- import System
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import Control.Monad
import Control.Exception

import Ensemble.Types
import ComponentModel.Types
import SPL.Types
import UseCaseModel.Types
import UseCaseModel.PrettyPrinter.Latex
import UseCaseModel.PrettyPrinter.LatexUserActions
import UseCaseModel.PrettyPrinter.XML

import qualified BasicTypes as Core

exportProduct :: FilePath -> FilePath -> InstanceModel -> IO ()
exportProduct s o p = 
 let 
  ucmodel = ucModel $ instanceAssetBase p 
  cmodel = components $ buildData $ instanceAssetBase p
 in do
  print "\n Use cases... " 
  print $ ucmodel

  exportUcmToLatex (o ++ "/use-cases.tex") (ucmodel)
  exportUcmToLatexUserActions (o ++ "/simplified-use-cases.tex") (ucmodel)
  exportUcmToXML (o ++ "/use-cases.xml") (ucmodel)

  print "\n Copying source files to output directory \n"
  print $ map (++ "\n") [fst c | c <- cmodel]

  putStrLn "we are going to export the source code"   
  exportSourceCode s o p

 
exportSourceCode :: FilePath -> FilePath -> InstanceModel -> IO ()
exportSourceCode s o p = 
 let bd = buildData $ instanceAssetBase p 
 in do
  if("all" `elem` (map fst (components bd))) 
   then copyAllFiles s o
   else do 
    copySourceFiles s o (components bd)
    exportBuildFile  (o ++ "/build.lst") (buildEntries bd)
    preprocessFiles (o ++ "/build.lst") (preProcessFiles bd) o
  putStrLn "we are going to remove components" 
  removeComponents s o p
  createPropertyFiles s o p


preprocessFiles :: String -> [String] -> String -> IO()
preprocessFiles _ [] _ = return()
preprocessFiles flags files outputFolder = do 
   pid <- runCommand ("java -jar preprocessor.jar \"" ++ flags ++ "\"" ++ (concatPaths files outputFolder))
   waitForProcess pid >>= exitWith
   
concatPaths :: [String] -> String -> String
concatPaths [] _ = ""
concatPaths (str:strs) outputFolder = " \"" ++ outputFolder </> str ++ "\"" ++ (concatPaths strs outputFolder)

-- -----------------------------------------------------------------------
-- Exports a use case model as a latex document.
-- -----------------------------------------------------------------------
exportUcmToLatex f ucm = 
 bracket (openFile f WriteMode)
         hClose
         (\h -> hPutStr h (show (ucmToLatex ucm)))

exportUcmToLatexUserActions f ucm = 
 bracket (openFile f WriteMode)
         hClose
         (\h -> hPutStr h (show (ucmToLatexUserActions ucm)))

exportUcmToXML f ucm = 
 bracket (openFile f WriteMode)
         hClose
         (\h -> hPutStr h (show (ucmToXML ucm)))

exportAspectInterfacesToLatex f ucm = 
  bracket (openFile f WriteMode)
         hClose
         (\h -> hPutStr h (show (aspectInterfacesToLatex ucm)))


-- ----------------------------------------------------------------------
-- Copy selected source files to the output directory
-- ----------------------------------------------------------------------
copySourceFiles source out [] = return ()
copySourceFiles source out (c:cs) = 
 do 
  createOutDir out c 
  copySourceFile source out c
  copySourceFiles source out cs

-- --------------------------------------------------------------------
-- Exports the list of build entries as a build file. The format of this 
-- build file should describe, for instance, which Eclipse plugins 
-- have to be considered when building an application. 
-- -------------------------------------------------------------------
exportBuildFile f es = 
 bracket (openFile f WriteMode)
         hClose
         (\h -> hPutStr h (concat [e ++ "\n" | e <- es]))

createOutDir out c =
 do
  print $ "Selected output dir: " ++ out
  print $ "Selected output component" ++ (snd c)
  let new = out </> (snd c)
  let newDir = dropFileName new
  print new 
  print newDir 
  createDirectoryIfMissing True newDir

-- copy a component from source directory to the out directory. 
-- Note that a component is a pair of ids (Id, Id), indicating the 
-- names of the input and output files. Usually they have the same 
-- name. 
copySourceFile source out c = 
 do 
  let old = source </> (fst c)
  let new = out </> (snd c)
  print ("Copying file " ++ old ++ " to " ++ new)
  copyFile old new
  

createPropertyFiles s o p = do
  print "----------------------------------------"
  print "creating property files                 " 
  print "----------------------------------------" 
  let bd = buildData $ instanceAssetBase p 
  let cs = components bd
  let ps = propertyFiles bd
  createPropertyFiles' s o cs ps 
 where 
  createPropertyFiles' s o cs [] = return ()
  createPropertyFiles' s o cs (p:ps) = do 
   let fs = [snd f | f <- cs, fst p == fst f]
   case fs of 
    [f] ->  bracket (openFile (o </> f) WriteMode)
                     hClose
                     (\h -> hPutStr h (concat [k ++ "=" ++ v ++ "\n" | (k,v) <- snd p]))
    otherwise -> print $ "file " ++ fst p ++ " not listed in the component model" 
   createPropertyFiles' s o cs ps 


removeComponents s o p = do
  print "----------------------------------------"
  print "removing components" 
  print "----------------------------------------" 
  let bd  = buildData $ instanceAssetBase p 
  let cs  = components bd
  let ecs = excludedComponents bd 
  removeComponents' s o cs ecs 
 where
  removeComponents' :: FilePath -> FilePath -> [ComponentMapping] -> [Component] -> IO ()
  removeComponents' _ _ _ [] = return()
  removeComponents' s o cs (ec:ecs) = do
   let file = [snd f | f <-cs, ec == fst f]
   case file of 
    [ ] -> return ()
    [f] -> removeComponent o f
   removeComponents' s o cs ecs 

removeComponent targetDir cmp = do 
 testFile <- doesFileExist (targetDir </> cmp)
 testDir  <- doesDirectoryExist (targetDir </> cmp)
 if (testFile)     then removeFile (targetDir </> cmp)
 else if (testDir) then removeDirectoryRecursive (targetDir </> cmp)
 else return ()

copyAllFiles source out = 
 do 
  createDirectoryIfMissing True out
  contents <- getDirectoryContents source
  copyAllFiles' out contents
 where 
  copyAllFiles' out []     = return ()
  copyAllFiles' out (f:fs) = do
     testDir <- doesDirectoryExist (source </> f) 
     if(testDir) 
      then 
       if(f == "." || f == "..") 
        then return () --print $ "skiping " ++ (source </> f)
        else do 
          -- print $ "subdir " ++ (source </> f) 
          copyAllFiles (source </> f) (out </> f)
      else 
        catch (copyFile (source </> f) (out </> f)) 
              (\e -> do let err = show (e::IOException) 
                        print $ "could not copy file " ++ source </> f ++ " " ++ err)  
     copyAllFiles' out fs

