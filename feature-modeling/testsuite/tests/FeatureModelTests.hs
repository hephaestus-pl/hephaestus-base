module FeatureModelTests where 

import FeatureModel.Types
import FeatureModel.FMTypeChecker

import Test.HUnit


simple, simple', simple'', simple'''   :: FeatureModel

simple = FeatureModel { 
 fmTree = Root a [Leaf b],
 fmConstraints = [] 
}

simple' = FeatureModel {
  fmTree = Root a [Leaf b, Leaf c],
  fmConstraints = [] 
}

simple'' = FeatureModel {
  fmTree = Root a [Leaf b, Leaf c, (Root d [Leaf d1, Leaf d2])],
  fmConstraints = [] 
}

simple''' = FeatureModel {
  fmTree = Root a [Leaf b, Leaf c, (Root d [Leaf d1, Leaf d2, (Root d3 [Leaf d31, Leaf d32])])],
  fmConstraints = [Not (FeatureRef "c")] 
}

a = Feature "a" "a" Mandatory "" BasicFeature []
  
b = Feature "b" "b" Optional "" BasicFeature []

c = Feature "c" "c" Mandatory "" BasicFeature [] 

d = Feature "d" "d" Optional "" OrFeature []

d1 = Feature "d1" "d1" Optional "" BasicFeature []

d2 = Feature "d2" "d2" Optional "" BasicFeature []

d3 = Feature "d3" "d3" Optional "" AlternativeFeature []

d31 = Feature "d31" "d31" Optional "" BasicFeature []

d32 = Feature "d32" "d32" Optional "" BasicFeature []

simpleSummary    = FMSummary 2 2
simpleSummary'   = FMSummary 3 4 
simpleSummary''  = FMSummary 6 8 
simpleSummary''' = FMSummary 9 12

-- the following test cases verify the transformations of feature models 
-- into propositional logic.

test1 = TestCase (assertEqual " Number of features for simpleTree "  simpleSummary  (summary simple)) 
test2 = TestCase (assertEqual " Number of features for simpleTree' " simpleSummary' (summary simple')) 
test3 = TestCase (assertEqual " Number of features for simpleTree'' " simpleSummary'' (summary simple'')) 
test4 = TestCase (assertEqual " Number of features for simpleTree''' " simpleSummary''' (summary simple''')) 

-- satisfiability tests 

test5 = TestCase (assertEqual " " True (isSatisfiable simple'''))

allTests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3, TestLabel "test4" test4]


