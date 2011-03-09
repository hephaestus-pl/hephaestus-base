{-# OPTIONS -fglasgow-exts #-}
module BusinessProcess.Types 
(BusinessProcessModel(BPM),
 BusinessProcess,
 FlowObject(FlowObject, Start, End, Proceed),
 Transition,
 mkTransition,
 Condition,
 objects,
 processes,
 (<*>)
)
where 

import Data.Generics

type Annotation = String
type Id = String
type Condition = String
type Name = String

type Parameter = (Name,Value)

data Value = Unbound | Value String
  deriving(Eq, Data, Typeable)

data Pointcut = Pointcut String | Empty
 deriving(Show, Data, Typeable)

data FlowObjectType = Activity | Gateway | Join | EmptyFlow
 deriving(Eq, Show, Data, Typeable)

data AdviceType = Before | After | Around
  deriving(Eq, Show, Data, Typeable)

data Transition = MkTransition { 
 startObject :: FlowObject, 
 endObject :: FlowObject, 
 condition :: String
} deriving (Data, Typeable)

data FlowObject = FlowObject {id' :: Id, type' :: FlowObjectType, annotations' :: [Annotation], parameters' :: [Parameter]} 
                  | Start 
                  | End
                  | Proceed
                  deriving (Data, Typeable)


data ProcessType = BasicProcess 
                 | Advice {advType :: AdviceType, pc :: Pointcut} 
                 deriving(Show, Data, Typeable)

data BusinessProcess = BusinessProcess {
   pid :: Id, 
   ptype :: ProcessType,
   objects :: [FlowObject], 
   transitions :: [Transition] 
} deriving(Show, Data, Typeable) 

data BusinessProcessModel = BPM { processes :: [BusinessProcess] } deriving(Show, Data, Typeable)

mkTransition :: FlowObject -> FlowObject -> String -> Maybe Transition
mkTransition _ Start _ = Nothing     -- error "A Start object could not end a transition"
mkTransition End _ _   = Nothing     -- error "An End object could not start a transition"
mkTransition s e c     
  | (s == e)  = Nothing
  | otherwise = Just (MkTransition s e c)

(<*>) :: BusinessProcess -> [(FlowObject, FlowObject, Condition)]
(<*>) bp = map (\(MkTransition x y z) -> (x, y, z)) (transitions bp)

(<+>) = (\(x, y, z) -> mkTransition x y z)

idflow :: FlowObject -> String
idflow (FlowObject i _ _ _) = i
idflow Start = "start"
idflow End = "end"
idflow Proceed = "proceed"

showParameters :: [Parameter] -> String
showParameters [] = ""
showParameters ps = " - Parameters: " ++ (init (concat (map (++",") (map showParameter ps))))

showParameter :: Parameter -> String
showParameter p = (fst p) ++ " - " ++ (value (snd p))

value :: Value -> String
value Unbound = "unbound"
value (Value v) = v

parameters :: FlowObject -> [Parameter]
parameters (FlowObject _ _ _ p) = p
parameters Start = []
parameters End = [] 
parameters Proceed = [] 

instance Show Transition where 
 show (MkTransition s e c) = show (show s, show e, c)

instance Eq BusinessProcess where 
 BusinessProcess i _ _ _  == BusinessProcess j _ _ _ = i == j

instance Eq FlowObject where 
 FlowObject i _ _ _ == FlowObject j _ _ _ = i == j 
 Start == Start = True
 End == End = True 
 Proceed == Proceed = True 
 _ == _  = False 

instance Show FlowObject where 
 show object = "(" ++  (idflow object) ++ (showParameters (parameters object)) ++ ")"

