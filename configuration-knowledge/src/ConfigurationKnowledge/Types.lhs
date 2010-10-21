\begin{code}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.SPL.ConfigurationKnowledge
-- Copyright   :  (c) Rodrigo Bonifacio 2008, 2009
-- License     :  LGPL
--
-- Maintainer  :  rba2@cin.ufpe.br
-- Stability   :  provisional
-- Portability :  portable
--
-- Configuration Knowledge in Haskell.
--
-----------------------------------------------------------------------------
{-# OPTIONS -fglasgow-exts #-}
module ConfigurationKnowledge.Types
where

import Data.Generics

import BasicTypes
import ComponentModel.Types
import UseCaseModel.Types
import RequirementModel.Types
import FeatureModel.Types (FeatureModel, FeatureConfiguration, FeatureExpression)

-- | A type that characterizes an initial representation  
--   of a SPL. Later, we should refactor this type, 
--   introducing new models, such as design, code, and tests.
data SPLModel = SPLModel {
      splFM  :: FeatureModel,
      splReq :: RequirementModel,  
      splUCM :: UseCaseModel, 
      splMappings :: ComponentModel 
} 
          
-- | A type for instances of an SPL. Note that, in this 
--   version, an instance model basically have a feature 
--   configuration and a use case model. Later, other models
--   might be introduced.
data InstanceModel = InstanceModel { 
      fc  :: FeatureConfiguration,
      req :: RequirementModel,   
      ucm :: UseCaseModel, 
      components :: [(Id, Id)], 
      buildEntries :: [String],
	  preProcessFiles :: [String]
} deriving (Data, Typeable)
                  

-- | 
-- Defines three levels of priority. 
-- Useful for ordering transformations.
-- 
data Priority = High | Medium | Low

-- | The transformation class defines a family of 
--   functions that, given an SPL and an Instance Model, 
--   apply some kind of transfomation to the instance model 
--   and then returns a refined version of it. 
class (Show t) => Transformation t  where 
 (<+>) :: t -> SPLModel  -> InstanceModel -> InstanceModel 

-- | A single configuration item. The idea is that, after 
--   evaluating each of the applicable configuration items, 
--   we generate a valid instance of the product line. 
data Configuration = 
 Configuration {
   expression :: FeatureExpression,   -- ^ if expression holds True for a product configuration...
   transformations :: [GenT]   	      -- ^ the list of transformations would be applied.
 } | 
 ConstrainedConfiguration { 
   expression :: FeatureExpression,   -- ^ if expression holds True for a product configuration...
   transformations :: [GenT],         -- ^ the list of transformations would be applied.
   required :: FeatureExpression,     -- ^ required expression for this configuration 
   provided :: FeatureExpression      -- ^ provided expression for this configuration
 }

constrained :: Configuration -> Bool 
constrained (ConstrainedConfiguration _ _ _ _) = True
constrained _ = False

data GenT = forall t . Transformation t => GenT t

-- | The model used to relate feature expressions 
--   to transformations. The configuration knowledge 
--   guides the 'building' process of SPL instances.
type ConfigurationKnowledge = [Configuration]

\end{code}
    

 