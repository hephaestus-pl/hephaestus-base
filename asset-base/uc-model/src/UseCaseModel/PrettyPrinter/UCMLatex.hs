
-----------------------------------------------------------------------------
-- |
-- Module      :  UseCaseModel.PrettyPrinter.Latex
-- Copyright   :  (c) Rodrigo Bonifacio 2008, 2009
-- License     :  LGPL
--
-- Maintainer  :  rba2@cin.ufpe.br
-- Stability   :  provisional
-- Portability :  portable
--
-- A Latex pretty printer for our Use Case Model data type. Useful for 
-- generating ps or pdf files from a use case model.
--
--
-----------------------------------------------------------------------------

module UseCaseModel.PrettyPrinter.UCMLatex (ucmToLatex, scenarioToLatex, adviceToLatex, flowToLatex)
where 

import Text.PrettyPrint.HughesPJ

import UseCaseModel.Types
 
ucmToLatex :: UseCaseModel -> Doc
ucmToLatex (UCM name ucs as) = vcat [ beginDocument name
                                    , ucsToLatex ucs
                                    , aspectsToLatex as
                                    , endDocument
                                    ]

beginDocument :: String -> Doc
beginDocument name =
 vcat [ text "%This Latex file is machine-generated by the Hephaestus\n"
      , text "\\documentclass[a4paper,11pt]{article}" 
      , text "\\newcommand{\\bl}{\\\\ \\hline}"
      , text ("\\title{Use Case Model Generated from " ++ name ++ "}")
      , text "\\begin{document}" 
      , text "\\maketitle"
      ]


ucsToLatex :: [UseCase] -> Doc 
ucsToLatex [] = empty
ucsToLatex (x:xs) = vcat ((text "\\section*{Use cases}") : (map ucToLatex (x:xs)))
 where 
   ucToLatex  uc = vcat ([ text ("\\subsection*{Use case " ++ (ucId uc) ++ "}") 
                         , text ("\\begin{itemize}") 
                         , text ("\\item {\\bf Name: }" ++ (ucName uc) )
                         , text ("\\item {\\bf Description: }" ++ (ucDescription uc))
                         , text ("\\end{itemize}" )  
                         ] ++ (map scenarioToLatex (ucScenarios uc)))

aspectsToLatex :: [AspectualUseCase] -> Doc
aspectsToLatex [] = empty
aspectsToLatex (x:xs)  = vcat ((text "\\section*{Aspectual use cases}") : (map aspectToLatex (x:xs)))
 where aspectToLatex a = vcat ([ text("\\subsection*{Aspectual use case " ++ (aspectId a) ++ "}")
                               , text("\\begin{itemize}")
                               , text("\\item{\\bf Name }" ++ (aspectName a) )
                               , text("\\end{itemize}")      
                              ] ++ (map adviceToLatex (advices a))) 

scenarioToLatex :: Scenario -> Doc
scenarioToLatex (Scenario i d f s t) = vcat ([ text ("\\subsubsection*{Scenario " ++ i ++ "}")
                                             , text ("\\begin{tabbing}")
                                             , text ("{\\bf Description:} \\= " ++ d ++ " \\\\")
                                             , hcat(text ("{\\bf From steps:} \\> ") : ((punctuate (text ",") (map idRefToLatex f)) ++ [text " \\\\"]))
                                             , hcat(text ("{\\bf To steps:} \\> ") : ((punctuate (text "," ) (map idRefToLatex t)) ++ [text " \\\\"]))
                                             , text ("\\end{tabbing}")
                                             ] ++ [(flowToLatex s)])

adviceToLatex :: Advice -> Doc
adviceToLatex adv =  vcat ([ text ("\\begin{tabbing}")
                           , text ("{\\bf Pointcut:} \\= " ++ (pc (advType adv)) ++ (concat (map show (pointCut adv)) ) ++ "\\\\" )
                           , text ("\\end{tabbing}")
                           ] ++ [(flowToLatex (aspectualFlow adv))])
 where 
  pc (Before) = "{\\bf BEFORE} "
  pc (After)  = "{\\bf AFTER} "    
  pc (Around) = "{\\bf AROUND} "

flowToLatex :: Flow -> Doc
flowToLatex [] = empty
flowToLatex (s:ss) = vcat [ text ("\\begin{tabular}{|p{0.4in}|p{1.2in}|p{1.2in}|p{1.2in}|p{1.2in}|}")
                          , text ("\\hline")
                          , text ("Id & User Action & Condition & System Response & Annotations \\bl ")
                          , vcat (map stepToLatex (s:ss))       
                          , text ("\\end{tabular}" ) 
                          ]

stepToLatex :: Step -> Doc
stepToLatex (Step i a s r an) = hcat [ text i
                                     , text " & " 
                                     , text a
                                     , text " & " 
                                     , text s 
                                     , text " & " 
                                     , text r 
                                     , text " & " 
                                     , hcat (punctuate (text "," ) (map text an))
                                     , text " \\bl "
                                     ]  

stepToLatex Proceed = text "\\multicolumn{5}{c}{{\\bf PROCEED}} \\bl "

idRefToLatex :: StepRef -> Doc
idRefToLatex (IdRef s) = text s
idRefToLatex (AnnotationRef s) = text s            
   
endDocument :: Doc
endDocument =  text "\\end{document}"