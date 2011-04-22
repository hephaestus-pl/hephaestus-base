module BusinessProcess.Parsers.XML.XmlBusinessProcess where

import Maybe

import BusinessProcess.Types

import BusinessProcess.Parsers.XML.JPDLXmlBinding

{-Returns the actual String or blank String-}
getMaybeString :: Maybe String -> String

getMaybeString (Just s) = s
getMaybeString Nothing = ""

{-Removes ocurrences of Nothing in a given list-}
discardNothingFromList :: [Maybe a] -> [a]

discardNothingFromList (h:t) = case h of
                                Nothing -> discardNothingFromList t
                                (Just y) -> y:discardNothingFromList t
discardNothingFromList [] = []

findFlowObject :: [FlowObject] -> String -> FlowObject -> FlowObject

findFlowObject flows name currentFlow | resultSet == [] = currentFlow
                                      | otherwise = head resultSet
                                    where  resultSet = discardNothingFromList $ map filter flows
                                                    where
                                                        filter (FlowObject a b c d) | a == name = (Just (FlowObject a b c d))
                                                                                    | otherwise = Nothing
                                                        filter Start = Nothing
                                                        filter End = Nothing




{-Convert from JPDL BPM to Hepheastus BPM Model-}
processDefinitionToBPM :: XMLProcessDefinition -> BusinessProcess

processDefinitionToBPM (XMLProcessDefinition    pdName
                                                pdActions
                                                pdSwimlanes
                                                pdStartState
                                                pdStates
                                                pdTaskNodes
                                                pdSuperStates
                                                pdProcessStates
                                                pdNodes
                                                pdForks
                                                pdJoins
                                                pdDecicisions
                                                pdEndStates
                                                pdScripts
                                                pdCreateTimers
                                                pdCancelTimers
                                                pdTasks
                                                pdEvents
                                                pdExceptionHandlers) = (BusinessProcess (getMaybeString pdName) (BasicProcess) buildFlowObjects buildTransitions)
                                                where
                                                    {-Flow objects functions-}
                                                    buildFlowObjects = [Start] ++ (map fst concatFlowObjectsTuples) ++ [End]
                                                    concatFlowObjectsTuples = convertStates ++ convertDecisions ++ convertNodes ++ convertTaskNodes ++ convertForks ++ convertJoins
                                                    {-Flow objects functions - States conversion-}
                                                    convertStates = [(startState pdStartState) ,(endState $ head pdEndStates)] ++ map convertState pdStates
                                                    convertState (XMLState stName stAction stScript stCreateTimer stCancelTimer stTransitions stEvents stTimers stExceptionHandlers) = ((FlowObject (getMaybeString stName) Activity [] []), stTransitions)
                                                    startState (XMLStartState ssName ssSwimLane ssTask ssTransitions ssEvents ssExceptionHandlers) = ((FlowObject (getMaybeString ssName) Activity [] []), ssTransitions)
                                                    endState (XMLEndState esName esEvents esExceptionHandlers) = ((FlowObject (getMaybeString esName) Activity [] []), [])
                                                    {-Flow objects functions - Decision conversion-}
                                                    convertDecisions = map convertDecision pdDecicisions
                                                    convertDecision (XMLDecision dName dHandler dTransitions dEvents dTimers dExceptionHandlers) =  ((FlowObject dName Gateway [] []), dTransitions)
                                                    {-Flow objects functions - Node conversion-}
                                                    convertNodes = map convertNode pdNodes
                                                    convertNode (XMLNode nName nAction nScript nCreateTimer nCancelTimer nTransitions nEvents nTimers nExceptionHandlers) =  ((FlowObject nName Activity [] []), nTransitions)
                                                    {-Flow objects functions - Task-Node conversion-}
                                                    convertTaskNodes = map convertTaskNode pdTaskNodes
                                                    convertTaskNode (XMLTaskNode tnName tnSignal tnCreateTasks tnEndTasks tnTasks tnTransitions tnEvents tnTimers tnExceptionHandlers) = ((FlowObject (getMaybeString tnName) Activity [] []), tnTransitions)
                                                    {-Flow objects functions - Fork conversion-}
                                                    convertForks = map convertFork pdForks
                                                    convertFork (XMLFork fName fScript fTransitions fEvents fTimers fExceptionHandlers) = ((FlowObject (getMaybeString fName) Gateway [] []), fTransitions)
                                                    {-Flow objects functions - Join conversion-}
                                                    convertJoins = map convertJoin pdJoins
                                                    convertJoin (XMLJoin jName jTransitions jEvents jTimers jExceptionHandlers)  = ((FlowObject jName Join [] []), jTransitions)
                                                    {-Transitions related functions-}
                                                    buildTransitions = discardNothingFromList $ [(mkTransition Start (fst $ startState pdStartState) ""),(mkTransition (fst $ endState $ head pdEndStates) End "")] ++ (map convertTransition $ concat (map createTransitionTuple $ concatFlowObjectsTuples))
                                                                          where
                                                                          createTransitionTuple (flowObject, transitions) = map buildTuple transitions
                                                                                                                            where
                                                                                                                            buildTuple transition = (transition, flowObject)
                                                    convertTransition ((XMLTransition trName trTo trActions trScripts trCreateTimers trCancelTimers trExceptionHandlers), flowObject)= mkTransition flowObject (findFlowObject  buildFlowObjects (getMaybeString trTo) flowObject) (getMaybeString trName)

