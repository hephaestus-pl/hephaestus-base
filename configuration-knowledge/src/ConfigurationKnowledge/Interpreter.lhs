\begin{code}
-----------------------------------------------------------------------------
-- |
-- Module      :  ConfigurationKnowledge.Interpreter
-- Copyright   :  (c) Rodrigo Bonifacio 2008, 2009
-- License     :  LGPL
--
-- Maintainer  :  rba2@cin.ufpe.br
-- Stability   :  provisional
-- Portability :  portable
--
-- Configuration Knowledge interpreter in Haskell.
--
-----------------------------------------------------------------------------
{-# OPTIONS -fglasgow-exts #-}

module ConfigurationKnowledge.Interpreter (build)
where

import UseCaseModel.Types
import BusinessProcess.Types
import RequirementModel.Types
import ConfigurationKnowledge.Types
import FeatureModel.Types (FeatureModel, FeatureConfiguration, eval)

-- | Instantiates a product from the input models. 
--   In more details, it calls each transformation ('tasks', obtained 
--   from the configuration knowledge 'ck') that should be applied to 
--   the given feature configuration ('fc').

build :: FeatureModel                 -- ^ SPL feature model
      -> FeatureConfiguration         -- ^ selection of features, which characterizes the product
      -> ConfigurationKnowledge       -- ^ relationships between features and transformations
      -> SPLModel                     -- ^ SPL assets
      -> InstanceModel                -- ^ resulting instance of the build process
build fm fc ck spl = stepRefinement ts spl emptyInstance
 where 
  ts            = tasks ck fc
  ucmodel       = splUCM spl
  bpmodel       = splBPM spl
  emptyUCM      = ucmodel { useCases = [] , aspects = [] }
  emptyBPM      = bpmodel { processes = [] }
  emptyReq      = RM { reqs = [] }
  emptyInstance = InstanceModel fc emptyReq emptyUCM emptyBPM [] [] []
 	
tasks :: ConfigurationKnowledge -> FeatureConfiguration -> [GenT]
tasks ck fc = concat [transformations c | c <- ck, eval fc (expression c)]

stepRefinement :: [GenT] -> SPLModel -> InstanceModel -> InstanceModel
stepRefinement [] splModel instanceModel = instanceModel
stepRefinement ((GenT t):ts) splModel instanceModel = stepRefinement ts splModel ((<+>) t splModel instanceModel)
 
\end{code}
    

 