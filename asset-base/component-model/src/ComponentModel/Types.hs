-----------------------------------------------------------------------------
-- |
-- Module      :  ComponentModel.Types
-- Copyright   :  (c) Rodrigo Bonifacio 2008, 2009
-- License     :  LGPL
--
-- Maintainer  :  rba2@cin.ufpe.br
-- Stability   :  provisional
-- Portability :  portable
--
-- A generic (and simple) component model in Haskell for product 
-- line development. Although simple, it can be used with the purpose 
-- of representing source code assets as file paths.
--
-----------------------------------------------------------------------------
{-# OPTIONS -fglasgow-exts #-}
module ComponentModel.Types
where 

import Data.Data -- Generics

import CommonUtils

type Component = String 
type Key = String
type Value = String 

data GeneratedBuildData = GeneratedBuildData {
  components :: [ComponentMapping], 
  buildEntries :: [Id],
  preProcessFiles :: [Component],
  excludedComponents :: [Component],
  propertyFiles :: [(Id, [(Key, Value)])]
} deriving(Data, Typeable)

type ComponentModel = [ComponentMapping]
type ComponentMapping = (Id, Component)





