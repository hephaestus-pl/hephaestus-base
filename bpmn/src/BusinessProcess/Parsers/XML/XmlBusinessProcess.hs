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

findFlowObject :: [FlowObject] -> String -> FlowObject

findFlowObject flows name = head $ discardNothingFromList $ map filter flows
                                where
                                filter (FlowObject a b c d) | a == name = (Just (FlowObject a b c d))
                                                            | otherwise = Nothing
                                                            | a /= name = Nothing
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
                                                pdDecicision
                                                pdEndStates
                                                pdScripts
                                                pdCreateTimers
                                                pdCancelTimers
                                                pdTasks
                                                pdEvents
                                                pdExceptionHandlers) = (BusinessProcess (getMaybeString pdName) (BasicProcess) buildFlowObjects buildTransitions)
                                                where
                                                    buildFlowObjects = [Start] ++ (map fst $ convertStates) ++ [End]
                                                    convertStates = [(startState pdStartState) ,(endState $ head pdEndStates)] ++ map convertState pdStates
                                                    convertState (XMLState stName stAction stScript stCreateTimer stCancelTimer stTransitions stEvents stTimers stExceptionHandlers) = ((FlowObject (getMaybeString stName) Activity [] []), stTransitions)
                                                    startState (XMLStartState ssName ssSwimLane ssTask ssTransitions ssEvents ssExceptionHandlers) = ((FlowObject (getMaybeString ssName) Activity [] []), ssTransitions)
                                                    endState (XMLEndState esName esEvents esExceptionHandlers) = ((FlowObject (getMaybeString esName) Activity [] []), [])
                                                    buildTransitions = discardNothingFromList $ [(mkTransition Start (fst $ startState pdStartState) ""),(mkTransition (fst $ endState $ head pdEndStates) End "")] ++ (map convertTransition $ concat (map createTransitionTuple $ convertStates))
                                                                          where
                                                                          createTransitionTuple (flowObject, transitions) = map buildTuple transitions
                                                                                                                            where
                                                                                                                            buildTuple transition = (transition, flowObject)
                                                    convertTransition ((XMLTransition trName trTo trActions trScripts trCreateTimers trCancelTimers trExceptionHandlers), flowObject)= mkTransition flowObject (findFlowObject  buildFlowObjects $ getMaybeString trTo) (getMaybeString trName)





--





--
