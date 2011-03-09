module BusinessProcess.Parsers.XML.XmlBusinessProcess where 

import Maybe

import BusinessProcess.Types

type JPDLName = String
type JPDLTarget = String
type NamedProcess = (String, BusinessProcess)

data JPDLBusinessModel [JPDLProcessDefinition] 
data JPDLProcessDefinition [JPDLNode]

data JPDLNodeType = JPDLStart | JPDLState | JPDLFork | JPDLJoin | JPDLEnd

data JPDLNode = JPDLNode JPDLName JPDLNodeType [JPDLTransition]

data JPDLTransition = JPDLTransition JPDLName JPDLTarget

processDefinitionToBPM :: JPDLProcessDefinition -> BusinessProcessModel
processDefinitionToBPM (JPDLProcessDefinition ns)= 
 let 
  namedProcesses = map jpdlNodeToNamedProcess ns
  transitions = concat $ map (jpdlTransitionToTransition namedProcesses) [n | n <- ns]
 in 


jpdlNodeToNamedProcess :: JPDLNode -> NamedProcess
jpdlNodeToNamedProcess (JPDLNode n t ts) = 
 case t of 
  JPDLStart -> (n, Start)
  JPDLState -> (n, FlowObject n Activity [] [])
  JPDLFork  -> (n, FlowObject n Gateway  [] []) 
  JPDLJoin  -> (n, FlowObject n Join     [] []) 
  JPDLEnd   -> (n, End)

jpdlNodeToListOfTransitions :: [NamedProcess] -> JPDLNode -> [Transition]
jpdlNodeToListOfTransitions nps (JPDLNode n t ts) = map (jpdlTransitionToTransition nps n) ts

jpdlTransitionToTransition :: [NamedProcess] -> JPDLName -> JPDLTransition -> Transition
jpdlTransitionToTransition nps n (JPDLTransition tn target) = 
 let 
  s = findObject n nps
  e = findObject target nps
 in mkTransition s e "" 

findObject :: JPDLName -> [NamedProcess] -> Maybe FlowObject
findObject n [] = Nothing
findObject n (p:ps) = 
 if (n == fst p) 
  then snd p
  else findObject n ps
       




   