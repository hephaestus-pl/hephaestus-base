
-----------------------------------------------------------------------------
-- |
-- Module      :  Transformations.ComponentModel
-- Copyright   :  (c) Rodrigo Bonifacio 2008, 2009
-- License     :  LGPL
--
-- Maintainer  :  rba2@cin.ufpe.br
-- Stability   :  provisional
-- Portability :  portable
--
-- Several transformations that instantiate a product build 
-- file. 
--
-----------------------------------------------------------------------------
module Transformations.ComponentModel
where 

import BasicTypes
import ConfigurationKnowledge.Types

-- | This datatype represents a transformation that retrieves 
--   a component name from a component id. The component name 
--   is retrieved from a file that maps an id to the name of 
--   the component. 
data SelectComponents = SelectComponents {
 componentIds :: [Id]  
}
 
instance Transformation SelectComponents where 
 (<+>) (SelectComponents ids) spl product = 
        let 
         scs = [(snd x, snd x) | x <- splMappings spl, fst x `elem` ids] 
         ics = components product
        in case scs of 
         [] -> product 
         otherwise -> product { components = ics ++ scs }
  
instance Show SelectComponents where 
 show (SelectComponents ids) = "selectComponents " ++ (show ids) 


-- | This datatype represents a transformation 
--   that retrieves and changes the the relative path of a component. 
--   Similarly to the SelectComponents transformation, the list of 
--   components is retrieved from a file that maps ids to compnent's name. 
data SelectAndMoveComponent = SelectAndMoveComponent {
 componentId :: Id, 
 targetName :: String
}  

instance Transformation SelectAndMoveComponent where 
 (<+>) (SelectAndMoveComponent i t) spl product = 
        let 
         scs = [snd x | x <- splMappings spl, fst x == i] 
         ics = components product
        in case scs of 
         [] -> product
         otherwise -> product { components = ics ++ [ (c, t) | c <- scs] }

instance Show SelectAndMoveComponent where 
 show (SelectAndMoveComponent i t) = "selectAndMove " ++ i ++ " to " ++ t 

-- | This data type represents a transformation 
--   that creates a new build entry in the product being 
--   configurated. 
data CreateBuildEntries = CreateBuildEntries {
  entries :: [String]
}

instance Transformation CreateBuildEntries where
 (<+>) (CreateBuildEntries e) spl product =
        let es = buildEntries product
        in product { buildEntries = es ++ e }

  
instance Show CreateBuildEntries where 
 show (CreateBuildEntries e) = "createBuildEntries " ++ (show e)
		
data PreProcessor = PreProcessor {
     paths :: [Path]  
}

instance Transformation PreProcessor where 
 (<+>) (PreProcessor e) spl product =
        let es = preProcessFiles product
        in product { preProcessFiles = es ++ e }

instance Show PreProcessor where 
 show (PreProcessor e) = "preProcessor " ++ (show e) 

