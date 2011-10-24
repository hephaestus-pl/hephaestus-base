module SPL.Transformations.BusinessProcess where

import List
import Maybe

import SPL.ConfigurationKnowledge.Types
import BusinessProcess.Types

data SelectBusinessProcess = SelectBusinessProcess {
  bpId :: Id                                                    
}

data BindParameter = BindParameter {
  name :: Name, 
  value :: Value
}

data EvaluateAdvice = EvaluateAdvice {
  advId :: Id                                     
}

instance Transformation SelectBusinessProcess where 
 (<+>) (SelectBusinessProcess i) spl product = 
  let 
   spl' = splBPM spl
   product' = bpm product
  in product { bpm = selectBusinessProcess i spl' product' }


instance Transformation BindParameter where 
 (<+>) (BindParameter n v) spl product = 
  let
   spl' = splBPM spl
   product' = bpm product
  in product { bpm = bindParameter n v spl' product' }

instance Transformation EvaluateAdvice where 
 (<+>) (EvaluateAdvice i) = 
  let 
   spl' = splBPM spl
   product' = bpm product
  in product { bpm = evaluateAdvice i spl' product' }

instance Show SelectBusinessProcess where 
 show (SelectBusinessProcess bpid) = "selecting a business process with id " ++ bpid

instance Show BindParameter where 
 show (BindParameter n v) = "binding parameter " ++ n ++ " with value " ++ v

instance Show EvaluateAdvice where 
 show (EvaluateAdvice i) = "evaluating advice " ++ i
  
selectBusinessProcess :: Id -> BusinessProcessModel -> BusinessProcessModel -> BusinessProcessModel
selectBusinessProcess id spl product = 
 let bps = [bp | bp <- (processes spl), (pid bp == id), bp `notElem` (processes product)]
 in case bps of
   [bp] -> BPM ([bp] ++ (processes product))
   [] -> product

bindParameter :: Name -> Value -> BusinessProcessModel -> BusinessProcessModel
bindParameter np vp bpm = 
  BPM (
       map (bindParameter' np vp) (processes bpm)
      )

bindParameter' :: Name -> Value -> BusinessProcess -> BusinessProcess
bindParameter' np vp bp = 
 BusinessProcess {
  pid = pid bp,
  ptype = ptype bp,
  objects = (map (applyParm np vp) (objects bp)),
  transitions = transitions bp
}
 
applyParm :: Name -> Value -> FlowObject -> FlowObject
applyParm np vp fo =
 FlowObject {
    id' = idflow fo,
    type' = typeFlow fo,
    annotations' = annotations fo,
    parameters' = ([(np,vp) | (i,j) <- (parameters fo), np == i ] ++ 
                   [(i,j)   | (i,j) <- (parameters fo), np /= i ])
}

evaluateAdvice :: Id -> BusinessProcessModel -> BusinessProcessModel -> BusinessProcessModel
evaluateAdvice advId spl product = 
 let advs = [a | a <- (processes spl), (pid a) == advId]
 in case advs of 
      [adv] -> BPM (map (evaluateAdvice' adv) (processes product))
      otherwise -> product

evaluateAdvice' :: BusinessProcess -> BusinessProcess -> BusinessProcess
evaluateAdvice' adv bp =
 let exist = [obj | obj <- (objects bp), (obj `matches` (pointcut adv))] /= []
 in case ptype adv of 
   BasicProcess      -> bp 
   (Advice After  _) -> if exist then (evaluateAfterAdvice adv bp) else bp
   (Advice Before _) -> if exist then (evaluateBeforeAdvice adv bp) else bp
   (Advice Around _) -> if exist then (evaluateAroundAdvice adv bp) else bp

evaluateAfterAdvice :: BusinessProcess -> BusinessProcess -> BusinessProcess
evaluateAfterAdvice adv bp = 
 BusinessProcess {
  pid = pid bp,
  ptype = ptype bp,
  objects = (objects bp) ++ [obj|obj <- (objects adv), not ((idflow obj) `elem` ["start","end"]) ] ,
  transitions = map (<+>)  
                 ([(i,j,k) | (i,j,k) <- ((<*>) bp), not (i `matches` (pointcut adv))] ++ 
                  [(i,y,z) | (i,j,k) <- ((<*>) bp), (x,y,z) <- startTransitions adv, i `matches` (pointcut adv)] ++
                  [(x,j,z) | (i,j,k) <- ((<*>) bp), (x,y,z) <- endTransitions adv, i `matches` (pointcut adv)] ++ 
                  [(x,y,z) | (x,y,z) <- ((<*>) adv), (x, y, z) `notElem` ((startTransitions adv) ++ (endTransitions adv))])
}

evaluateBeforeAdvice :: BusinessProcess -> BusinessProcess -> BusinessProcess
evaluateBeforeAdvice adv bp = 
 BusinessProcess {
  pid = pid bp,
  ptype = ptype bp,
  objects = nub ((objects bp) ++ (objects adv)), 
  transitions = map (<+>)
                 ([(i,j,k) | (i,j,k) <- ((<*>) bp), not (j `matches` (pointcut adv))] ++ 
                  [(i,y,z) | (i,j,k) <- ((<*>) bp), (x,y,z) <- startTransitions adv, j `matches` (pointcut adv)] ++
                  [(x,j,z) | (i,j,k) <- ((<*>) bp), (x,y,z) <- endTransitions adv, j `matches` (pointcut adv)] ++ 
                  [(x,y,z) | (x,y,z) <- ((<*>) adv), (x, y, z) `notElem` ((startTransitions adv) ++ (endTransitions adv))])
}
 
evaluateAroundAdvice :: BusinessProcess -> BusinessProcess -> BusinessProcess
evaluateAroundAdvice adv bp =
 let objAnnot = [ obj | obj <- (objects bp), obj `matches` (pointcut adv)];
     transitionsTemp = 
      ([(i,j,k) | (i,j,k) <- ((<*>) bp), not (i `matches` (pointcut adv)), not (j `matches` (pointcut adv)) ] ++
       [(i,y,z) | (i,j,k) <- ((<*>) bp), (x,y,z) <- startTransitions adv, j `matches` (pointcut adv)] ++
       [(x,j,z) | (i,j,k) <- ((<*>) bp), (x,y,z) <- endTransitions adv, i `matches` (pointcut adv)] ++
       [(x,y,z) | (x,y,z) <- ((<*>) adv), (x, y, z) `notElem` ((startTransitions adv) ++ (endTransitions adv))]);
     objProceed      = [ obj | obj <- (objects adv), obj == Proceed ]
 in case objProceed of
     -- com Proceed
     [objProceed] -> bp {
                         objects = (objects bp) ++ [obj|obj <- (objects adv), not (obj `elem` [Start, End, Proceed]) ],
                         transitions = map (<+>)
                                       ([(i,obj,k) | (i,j,k) <- transitionsTemp, obj <- objAnnot, j == Proceed ] ++
                                        [(obj,j,k) | (i,j,k) <- transitionsTemp, obj <- objAnnot, i == Proceed ] ++
                                        [(i,j,k) | (i,j,k) <- transitionsTemp, i /= Proceed, j /= Proceed ])
                        }
     -- sem Proceed
     otherwise -> 
         bp {
             objects = [obj|obj <- (objects bp), obj `notElem` objAnnot] ++ [obj|obj <- (objects adv), not (obj `elem` [Start, End]) ],
             transitions = map (<+>) [(i,j,k) | (i,j,k) <- transitionsTemp]
            }

evaluateAdviceQ :: Id -> BusinessProcessModel -> BusinessProcessModel -> BusinessProcessModel
evaluateAdviceQ advId spl product = 
 let advs = [a | a <- (processes spl), (pid a) == advId]
 in case advs of 
      [adv] -> BPM (map (evaluateAdviceQ' adv) (processes product))
      otherwise -> product
 

evaluateAdviceQ' :: BusinessProcess -> BusinessProcess -> BusinessProcess
evaluateAdviceQ' adv bpProduct = 
 let objs = [obj | obj <- (objects bpProduct), (obj `matches` (pointcut adv))]
 in case objs of 
      [] -> bpProduct     
      otherwise -> evaluateAdviceQ'' adv objs bpProduct


evaluateAdviceQ'' :: BusinessProcess -> [FlowObject] -> BusinessProcess -> BusinessProcess
evaluateAdviceQ'' _ [] bpProduct = bpProduct
evaluateAdviceQ'' adv (o:os) bpProduct =
 case ptype adv of 
   BasicProcess -> bpProduct
   (Advice After  _) -> evaluateAdviceQ'' advRen os (evaluateAfterAdviceQ adv bpProduct o)
   (Advice Before _) -> evaluateAdviceQ'' advRen os (evaluateBeforeAdviceQ adv bpProduct o)
   (Advice Around _) -> evaluateAdviceQ'' advRen os (evaluateAroundAdviceQ adv bpProduct o)
   where
   advRen = adv {
                 objects = [rename oa | oa <- objects adv],
                 transitions = map (<+>) [(rename i, rename j,k)| (i,j,k) <- ((<*>) adv)]
                }
   rename obj 
     | (obj `notElem` [Start, End, Proceed]) = obj {id' = (idflow obj) ++ "'"}
     | otherwise = obj

evaluateAfterAdviceQ :: BusinessProcess -> BusinessProcess -> FlowObject -> BusinessProcess
evaluateAfterAdviceQ adv bp objAnn = 
 BusinessProcess {
  pid = pid bp,
  ptype = ptype bp,
--  objects = nub ((objects bp) ++ (objects adv)), 
  objects = (objects bp) ++ [obj | obj <- (objects adv), obj `notElem` [Start, End]] ,
  transitions = map (<+>) 
                ([(i,j,k) | (i,j,k) <- ((<*>) bp), i /= objAnn] ++ 
                 [(i,y,z) | (i,j,k) <- ((<*>) bp), (x,y,z) <- startTransitions adv, i == objAnn] ++
                 [(x,j,z) | (i,j,k) <- ((<*>) bp), (x,y,z) <- endTransitions adv, i == objAnn] ++ 
                 [(x,y,z) | (x,y,z) <- ((<*>) adv), (x, y, z) `notElem` ((startTransitions adv) ++ (endTransitions adv))])
}

evaluateBeforeAdviceQ :: BusinessProcess -> BusinessProcess -> FlowObject -> BusinessProcess
evaluateBeforeAdviceQ adv bp objAnn = 
 BusinessProcess {
  pid = pid bp,
  ptype = ptype bp,
--  objects = nub ((objects bp) ++ (objects adv)), 
  objects = (objects bp) ++ [obj | obj <- (objects adv), obj `notElem` [Start, End]] ,
  transitions = map (<+>)
                ([(i,j,k) | (i,j,k) <- ((<*>) bp), j /= objAnn] ++ 
                 [(i,y,z) | (i,j,k) <- ((<*>) bp), (x,y,z) <- startTransitions adv, j == objAnn] ++
                 [(x,j,z) | (i,j,k) <- ((<*>) bp), (x,y,z) <- endTransitions adv, j == objAnn] ++ 
                 [(x,y,z) | (x,y,z) <- ((<*>) adv), (x, y, z) `notElem` ((startTransitions adv) ++ (endTransitions adv))])
}
 
evaluateAroundAdviceQ :: BusinessProcess -> BusinessProcess -> FlowObject -> BusinessProcess
evaluateAroundAdviceQ adv bp objAnn =
 let objsAnnot = [ o | o <- (objects bp), o == objAnn];
     transitionsTemp = ([(i,j,k) | (i,j,k) <- ((<*>) bp), i /= objAnn, j /= objAnn] ++
                        [(i,y,z) | (i,j,k) <- ((<*>) bp), (x,y,z) <- startTransitions adv, j == objAnn] ++
                        [(x,j,z) | (i,j,k) <- ((<*>) bp), (x,y,z) <- endTransitions adv, i == objAnn] ++
                        [(x,y,z) | (x,y,z) <- ((<*>) adv), (x, y, z) `notElem` ((startTransitions adv) ++ (endTransitions adv))]);
     objProceed      = [ obj | obj <- (objects adv), obj == Proceed ]
 in case objProceed of
     -- com Proceed
     [objProceed] -> bp {
                         objects = (objects bp) ++ [obj | obj <- (objects adv), obj `notElem` [Start, End, Proceed]],
                         transitions = map (<+>)                                       ([(i,obj,k) | (i,j,k) <- transitionsTemp, obj <- objsAnnot, j == Proceed ] ++
                                        [(obj,j,k) | (i,j,k) <- transitionsTemp, obj <- objsAnnot, i == Proceed ] ++
                                        [(i,j,k) | (i,j,k) <- transitionsTemp, i /= Proceed, j /= Proceed ])
                        }
     -- sem Proceed
     otherwise -> bp {
                       objects = [obj | obj <- (objects bp), obj `notElem` objsAnnot] ++ 
                                 [obj | obj <- (objects adv), obj `notElem` [Start, End] ],
                       transitions = map (<+>) [(i,j,k) | (i,j,k) <- transitionsTemp]
                     }
                   
matches :: FlowObject -> Pointcut -> Bool
matches fo (Pointcut pc) = pc `elem` (annotations fo)
matches fo (Empty) = False

startTransitions :: BusinessProcess -> [(FlowObject, FlowObject, Condition)] 
startTransitions bp = [(i, j, k) | (i, j, k) <- ((<*>) bp), i == Start] 
  
endTransitions :: BusinessProcess -> [(FlowObject, FlowObject, Condition)]
endTransitions bp = [(i, j, k) | (i, j, k) <- ((<*>) bp), j == End] 

