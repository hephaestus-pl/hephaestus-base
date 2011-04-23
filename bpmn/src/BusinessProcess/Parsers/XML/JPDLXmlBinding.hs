-----------------------------------------------------------------------------
--
-- Module      :  BusinessProcess.Parsers.XML.JPDLXmlBinding
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  rbonifacio123@gmail.com
-- Stability   :  Experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module BusinessProcess.Parsers.XML.JPDLXmlBinding   where

import Text.XML.HXT.Core
import Maybe

data XMLAssignment = XMLAssignment  {
    aClass :: Maybe String,
    aConfig_type :: Maybe String,
    aExpression :: Maybe String
} deriving (Eq, Show)

data XMLSwimLane = XMLSwimLane {
    slName :: Maybe String,
    slAssignments :: [XMLAssignment]
} deriving (Eq, Show)

data XMLController = XMLController {
    cClass :: Maybe String,
    cConfig_type :: Maybe String
} deriving (Eq, Show)

data XMLAction = XMLAction {
    acAcceptPropagatedEvents :: Maybe String,
    acClass :: Maybe String,
    acConfigType :: Maybe String,
    acName :: Maybe String,
    acRefName :: Maybe String
} deriving (Eq, Show)

data XMLVariable = XMLVariable {
    vName :: Maybe String,
    vAccess :: Maybe String,
    vMappedName :: Maybe String
} deriving (Eq, Show)

data XMLScript = XMLScript {
    sName :: Maybe String,
    sAcceptPropagatedEvents :: Maybe String,
    sExpression :: Maybe String,
    sVariables :: [XMLVariable]
} deriving (Eq, Show)

data XMLCancelTimer = XMLCancelTimer {
   caName :: String {-Obrigatorio-}
} deriving (Eq, Show)

data XMLCreateTimer = XMLCreateTimer {
    crName :: Maybe String,
    crDueDate :: String,  {-Obrigatorio-}
    crRepeat :: Maybe String,
    crTransition :: Maybe String,
    crAction :: Maybe XMLAction,
    crScript :: Maybe XMLScript
} deriving (Eq, Show)

data XMLEvent = XMLEvent {
    eType :: String, {-Obrigatorio-}
    eActions :: [XMLAction],
    eScripts :: [XMLScript],
    eCreateTimers :: [XMLCreateTimer],
    eCancelTimers :: [XMLCancelTimer]
} deriving (Eq, Show)

data XMLTimer = XMLTimer {
    tiName :: Maybe String,
    tiDueDate :: String,  {-Obrigatorio-}
    tiRepeat :: Maybe String,
    tiTransition :: Maybe String,
    tiAction :: Maybe XMLAction,
    tiScript :: Maybe XMLScript
} deriving (Eq, Show)

data XMLExceptionHandler = XMLExceptionHandler {
    ehExceptionClass :: Maybe String,
    ehActions :: [XMLAction],
    ehScripts :: [XMLScript]
} deriving (Eq, Show)

data XMLTask = XMLTask {
    tBlocking :: Maybe String,
    tDescription :: Maybe String,
    tDuedate :: Maybe String,
    tName :: Maybe String,
    tPriority :: Maybe String,
    tSignalling :: Maybe String,
    tSwimlane :: Maybe String,
    tAssignment :: Maybe XMLAssignment,
    tController :: Maybe XMLController,
    tEvents :: [XMLEvent],
    tTimers :: [XMLTimer],
    tExceptionHandlers :: [XMLExceptionHandler]
} deriving (Eq, Show)

data XMLStartState = XMLStartState {
    ssName :: Maybe String,
    ssSwimLane :: Maybe String,
    ssTask :: Maybe XMLTask,
    ssTransitions :: [XMLTransition],
    ssEvents :: [XMLEvent],
    ssExceptionHandlers :: [XMLExceptionHandler]
} deriving (Eq, Show)

data XMLTransition = XMLTransition {
    trName :: Maybe String,
    trTo :: Maybe String,
    trActions :: [XMLAction],
    trScripts :: [XMLScript],
    trCreateTimers :: [XMLCreateTimer],
    trCancelTimers :: [XMLCancelTimer],
    trExceptionHandlers :: [XMLExceptionHandler]
} deriving (Eq, Show)

data XMLState = XMLState {
    stName :: Maybe String,
    stAction :: Maybe XMLAction,
    stScript :: Maybe XMLScript,
    stCreateTimer :: Maybe XMLCreateTimer,
    stCancelTimer :: Maybe XMLCancelTimer,
    stTransitions :: [XMLTransition],
    stEvents :: [XMLEvent],
    stTimers :: [XMLTimer],
    stExceptionHandlers :: [XMLExceptionHandler]
} deriving (Eq, Show)

data XMLTaskNode = XMLTaskNode {
    tnName :: Maybe String,
    tnSignal :: Maybe String,
    tnCreateTasks :: Maybe String,
    tnEndTasks :: Maybe String,
    tnTasks :: [XMLTask],
    tnTransitions :: [XMLTransition],
    tnEvents :: [XMLEvent],
    tnTimers :: [XMLTimer],
    tnExceptionHandlers :: [XMLExceptionHandler]
} deriving (Eq, Show)

data XMLHandler = XMLHandler {
    hClass :: String,{-obrigatorio-}
    hConfigType :: Maybe String
} deriving (Eq, Show)

data XMLDecision = XMLDecision {
    dName :: String,{-Obrigatorio-}
    dHandler :: Maybe XMLHandler,
    dTransitions :: [XMLTransition],
    dEvents :: [XMLEvent],
    dTimers :: [XMLTimer],
    dExceptionHandlers :: [XMLExceptionHandler]
} deriving (Eq, Show)

data XMLEndState = XMLEndState {
    esName :: Maybe String,
    esEvents :: [XMLEvent],
    esExceptionHandlers :: [XMLExceptionHandler]
} deriving (Eq, Show)

data XMLFork = XMLFork {
    fName :: Maybe String,
    fScript :: Maybe XMLScript,
    fTransitions :: [XMLTransition],
    fEvents :: [XMLEvent],
    fTimers :: [XMLTimer],
    fExceptionHandlers :: [XMLExceptionHandler]
} deriving (Eq, Show)

data XMLJoin = XMLJoin {
    jName :: String,{-Obrigatorio-}
    jTransitions :: [XMLTransition],
    jEvents :: [XMLEvent],
    jTimers :: [XMLTimer],
    jExceptionHandlers :: [XMLExceptionHandler]
} deriving (Eq, Show)

data XMLNode = XMLNode {
    nName :: String, {-Obrigatorio-}
    nAction :: Maybe XMLAction,
    nScript :: Maybe XMLScript,
    nCreateTimer :: Maybe XMLCreateTimer,
    nCancelTimer :: Maybe XMLCancelTimer,
    nTransitions :: [XMLTransition],
    nEvents :: [XMLEvent],
    nTimers :: [XMLTimer],
    nExceptionHandlers :: [XMLExceptionHandler]
} deriving (Eq, Show)

data XMLSubProcess = XMLSubProcess {
    sbName :: String,{-Obrigatorio-}
    sbVersion :: Maybe Integer
} deriving (Eq, Show)

data XMLProcessState = XMLProcessState {
    psName :: String,{-Obrigatorio-}
    psSubProcess :: Maybe XMLSubProcess,
    psVariabes :: [XMLVariable],
    psTransitions :: [XMLTransition],
    psEvents :: [XMLEvent],
    psTimers :: [XMLTimer],
    psExceptionHandlers :: [XMLExceptionHandler]
} deriving (Eq, Show)

data XMLSuperState = XMLSuperState {
    spName :: Maybe String,
    spStates :: [XMLState],
    spTaskNodes :: [XMLTaskNode],
    spSuperStates :: [XMLSuperState],
    spProcessStates :: [XMLProcessState],
    spNodes :: [XMLNode],
    spForks :: [XMLFork],
    spJoins :: [XMLJoin],
    spDecicision :: [XMLDecision],
    spEndStates :: [XMLEndState],
    spTransitions :: [XMLTransition],
    spEvents :: [XMLEvent],
    spTimers :: [XMLTimer],
    spExceptionHandlers :: [XMLExceptionHandler]
} deriving (Eq, Show)

data XMLProcessDefinition = XMLProcessDefinition {
    pdActions :: [XMLAction],
    pdCancelTimers :: [XMLCancelTimer],
    pdCreateTimers :: [XMLCreateTimer],
    pdDecicisions :: [XMLDecision],
    pdEndStates :: [XMLEndState],
    pdEvents :: [XMLEvent],
    pdExceptionHandlers :: [XMLExceptionHandler],
    pdForks :: [XMLFork],
    pdJoins :: [XMLJoin],
    pdName :: Maybe String,
    pdNodes :: [XMLNode],
    pdProcessStates :: [XMLProcessState],
    pdScripts :: [XMLScript],
    pdStartState :: XMLStartState,
    pdStates :: [XMLState],
    pdSuperStates :: [XMLSuperState],
    pdSwimlanes :: [XMLSwimLane],
    pdTaskNodes :: [XMLTaskNode],
    pdTasks :: [XMLTask]
} deriving (Eq, Show)

{- Utilities -}


uncurry19 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o -> p -> q -> r -> s -> t) -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) -> t
uncurry19 fn (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)	= fn a b c d e f g h i j k l m n o p q r s

uncurry14 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o)  -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n) -> o
uncurry14 fn (a, b, c, d, e, f, g, h, i, j, k, l, m, n)	= fn a b c d e f g h i j k l m n

uncurry12 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m)  -> (a, b, c, d, e, f, g, h, i, j, k, l) -> m
uncurry12 fn (a, b, c, d, e, f, g, h, i, j, k, l)	= fn a b c d e f g h i j k l

uncurry9 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> (a, b, c, d, e, f, g, h, i) -> j
uncurry9 fn (a, b, c, d, e, f, g, h, i)	= fn a b c d e f g h i

uncurry7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> (a, b, c, d, e, f, g) -> h
uncurry7 fn (a, b, c, d, e, f, g)	= fn a b c d e f g

uncurry6 :: (a -> b -> c -> d -> e -> f -> g) -> (a, b, c, d, e, f) -> g
uncurry6 fn (a, b, c, d, e, f) = fn a b c d e f

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 fn (a, b, c, d, e) = fn a b c d e

{------------Picklers ------------------}


instance XmlPickler XMLProcessDefinition where
    xpickle = xpXMLProcessDefinition

instance XmlPickler XMLSwimLane where
    xpickle = xpXMLSwimLane

instance XmlPickler XMLAssignment where
    xpickle = xpXMLAssignment

instance XmlPickler XMLAction where
    xpickle = xpXMLAction

instance XmlPickler XMLScript where
    xpickle = xpXMLScript

instance XmlPickler XMLVariable where
    xpickle = xpXMLVariable

instance XmlPickler XMLController where
    xpickle = xpXMLController

instance XmlPickler XMLCancelTimer where
    xpickle = xpXMLCancelTimer

instance XmlPickler XMLCreateTimer where
    xpickle = xpXMLCreateTimer

instance XmlPickler XMLEvent where
    xpickle = xpXMLEvent

instance XmlPickler XMLTimer where
    xpickle = xpXMLTimer

instance XmlPickler XMLExceptionHandler where
    xpickle = xpXMLExceptionHandler

instance XmlPickler XMLTask where
    xpickle = xpXMLTask

instance XmlPickler XMLStartState where
    xpickle = xpXMLStartState

instance XmlPickler XMLTaskNode where
    xpickle = xpXMLTaskNode

instance XmlPickler XMLHandler where
    xpickle = xpXMLHandler

instance XmlPickler XMLDecision where
    xpickle = xpXMLDecision

instance XmlPickler XMLEndState where
    xpickle = xpXMLEndState

instance XmlPickler XMLFork where
    xpickle = xpXMLFork

instance XmlPickler XMLJoin where
    xpickle = xpXMLJoin

instance XmlPickler XMLNode where
    xpickle = xpXMLNode

instance XmlPickler XMLSubProcess where
    xpickle = xpXMLSubProcess

instance XmlPickler XMLProcessState where
    xpickle = xpXMLProcessState

instance XmlPickler XMLSuperState where
    xpickle = xpXMLSuperState

xpXMLProcessDefinition :: PU XMLProcessDefinition

xpXMLProcessDefinition = xpElem "process-definition" $
                         xpWrap (
                                    uncurry19 XMLProcessDefinition,
                                    \ t -> (
                                            pdActions t,
                                            pdCancelTimers t,
                                            pdCreateTimers t,
                                            pdDecicisions t,
                                            pdEndStates t,
                                            pdEvents t,
                                            pdExceptionHandlers t,
                                            pdForks t,
                                            pdJoins t,
                                            pdName t,
                                            pdNodes t,
                                            pdProcessStates t,
                                            pdScripts t,
                                            pdStartState t,
                                            pdStates t,
                                            pdSuperStates t,
                                            pdSwimlanes t,
                                            pdTaskNodes t,
                                            pdTasks t
                                            )
                                 ) $
                         xp19Tuple  (xpList xpXMLAction)
                                    (xpList xpXMLCancelTimer)
                                    (xpList xpXMLCreateTimer)
                                    (xpList xpXMLDecision)
                                    (xpList xpXMLEndState)
                                    (xpList xpXMLEvent)
                                    (xpList xpXMLExceptionHandler)
                                    (xpList xpXMLFork)
                                    (xpList xpXMLJoin)
                                    (xpOption $ xpAttr "name" xpText0)
                                    (xpList xpXMLNode)
                                    (xpList xpXMLProcessState)
                                    (xpList xpXMLScript)
                                    (xpXMLStartState)
                                    (xpList xpXMLState)
                                    (xpList xpXMLSuperState)
                                    (xpList xpXMLSwimLane)
                                    (xpList xpXMLTaskNode)
                                    (xpList xpXMLTask)



xpXMLSwimLane :: PU XMLSwimLane
xpXMLSwimLane = xpElem "swimlane" $
                xpWrap (
                            uncurry XMLSwimLane,
                            \ t -> (
                                    slName t,
                                    slAssignments t
                                    )
                       ) $
                xpPair      (xpOption $ xpAttr "name" xpText0)
                            (xpList xpXMLAssignment)


xpXMLAssignment	:: PU XMLAssignment
xpXMLAssignment = xpElem "assignment" $
                  xpWrap (
                            uncurry3  XMLAssignment,
                            \ t -> (
                                    aClass t,
                                    aConfig_type t,
                                    aExpression t
                                    )
                          )  $
                  xpTriple (xpOption $ xpAttr "class" xpText0)
                           (xpOption $ xpAttr "config-type" xpText0)
                           (xpOption $ xpAttr "expression" xpText0)


xpXMLAction :: PU XMLAction
xpXMLAction =   xpElem "action" $
                xpWrap (
                            uncurry5 XMLAction,
                            \ t -> (
                                   acAcceptPropagatedEvents t,
                                   acClass t,
                                   acConfigType t,
                                   acName t,
                                   acRefName t
                                   )
                        ) $
                xp5Tuple    (xpOption $ xpAttr "accept-propagated-events" xpText0)
                            (xpOption $ xpAttr "class" xpText0)
                            (xpOption $ xpAttr "config-type" xpText0)
                            (xpOption $ xpAttr "name" xpText0)
                            (xpOption $ xpAttr "ref-name" xpText0)

xpXMLScript :: PU XMLScript
xpXMLScript =   xpElem "script" $
                xpWrap (
                        uncurry4 XMLScript,
                            \ t -> (
                                    sName t,
                                    sAcceptPropagatedEvents t,
                                    sExpression t,
                                    sVariables t
                                   )
                       ) $
                xp4Tuple    (xpOption $ xpAttr "name" xpText0)
                            (xpOption $ xpAttr "accept-propagated-events" xpText0)
                            (xpOption $ xpElem "expression" xpText0)
                            (xpList xpXMLVariable)


xpXMLVariable :: PU XMLVariable
xpXMLVariable =     xpElem "variable" $
                    xpWrap (
                                uncurry3 XMLVariable,
                                \ t -> (
                                        vName t,
                                        vAccess t,
                                        vMappedName t
                                       )
                       ) $
                xpTriple    (xpOption $ xpAttr "name" xpText0)
                            (xpOption $ xpAttr "access" xpText0)
                            (xpOption $ xpAttr "mapped-name" xpText0)


xpXMLController :: PU XMLController
xpXMLController =   xpElem "controller" $
                    xpWrap (
                                uncurry XMLController,
                                \ t -> (
                                        cClass t,
                                        cConfig_type t
                                       )
                       ) $
                    xpPair  (xpOption $ xpAttr "class" xpText0)
                            (xpOption $ xpAttr "config-type" xpText0)

xpXMLCancelTimer :: PU XMLCancelTimer
xpXMLCancelTimer =  xpElem "cancel-timer" $
                    xpWrap (
                             XMLCancelTimer ,
                             \ t -> (
                                     caName t
                                    )
                            ) $
                    xpAttr "name" xpText0



xpXMLCreateTimer :: PU XMLCreateTimer
xpXMLCreateTimer =  xpElem "create-timer" $
                    xpWrap (
                             uncurry6 XMLCreateTimer,
                             \ t -> (
                                     crName t,
                                     crDueDate t,
                                     crRepeat t,
                                     crTransition t,
                                     crAction t,
                                     crScript t
                                    )
                            ) $
                     xp6Tuple   (xpOption $ xpAttr "name" xpText0)
                                (xpAttr "duedate" xpText)
                                (xpOption $ xpAttr "repeat" xpText0)
                                (xpOption $ xpAttr "transition" xpText0)
                                (xpOption xpXMLAction)--xpChoice
                                (xpOption xpXMLScript)


xpXMLEvent :: PU XMLEvent
xpXMLEvent =    xpElem "event" $
                xpWrap (
                         uncurry5 XMLEvent,
                         \ t -> (
                                 eType t,
                                 eActions t,
                                 eScripts t,
                                 eCreateTimers t,
                                 eCancelTimers t
                                )
                        ) $
                 xp5Tuple   (xpAttr "type" xpText)
                          --(xpList $ xpChoice xpXMLAction $ xpChoice xpXMLScript $ xpChoice xpXMLCreateTimer xpXMLCancelTimer) --xpChoice
                            (xpList xpXMLAction)
                            (xpList xpXMLScript)
                            (xpList xpXMLCreateTimer)
                            (xpList xpXMLCancelTimer)


xpXMLTimer :: PU XMLTimer
xpXMLTimer =    xpElem "timer" $
                xpWrap (
                         uncurry6 XMLTimer,
                         \ t -> (
                                 tiName t,
                                 tiDueDate t,
                                 tiRepeat t,
                                 tiTransition t,
                                 tiAction t,
                                 tiScript t
                                )
                        ) $
                 xp6Tuple   (xpOption $ xpAttr "name" xpText0)
                            (xpAttr "duedate" xpText)
                            (xpOption $ xpAttr "repeat" xpText0)
                            (xpOption $ xpAttr "transition" xpText0)
                            --(xpChoice xpXMLAction xpXMLScript) --xpChoice
                            (xpOption xpXMLAction)
                            (xpOption xpXMLScript)


xpXMLExceptionHandler :: PU XMLExceptionHandler
xpXMLExceptionHandler = xpElem "exception-handler" $
                        xpWrap (
                                 uncurry3 XMLExceptionHandler,
                                 \ t -> (
                                         ehExceptionClass t,
                                         ehActions t,
                                         ehScripts t
                                        )
                                ) $
                        xpTriple  (xpOption $ xpAttr "exception-class" xpText0)
                                  --(xpList xpChoice xpXMLAction xpXMLScript)--xpChoice
                                  (xpList xpXMLAction)
                                  (xpList xpXMLScript)


xpXMLTask :: PU XMLTask
xpXMLTask = xpElem "task" $
            xpWrap (
                        uncurry12 XMLTask,
                        \ t -> (
                                tBlocking t,
                                tDescription t,
                                tDuedate t,
                                tName t,
                                tPriority t,
                                tSignalling t,
                                tSwimlane t,
                                tAssignment t,
                                tController t,
                                tEvents t,
                                tTimers t,
                                tExceptionHandlers t
                             )
                    ) $
            xp12Tuple   (xpOption $ xpAttr "blocking" xpText0)
                        (xpOption $ xpAttr "description" xpText0)
                        (xpOption $ xpAttr "duedate" xpText0)
                        (xpOption $ xpAttr "name" xpText0)
                        (xpOption $ xpAttr "priority" xpText0)
                        (xpOption $ xpAttr "signalling" xpText0)
                        (xpOption $ xpAttr "swimlane" xpText0)
                        (xpOption xpXMLAssignment)
                        (xpOption xpXMLController)
                        (xpList xpXMLEvent)
                        (xpList xpXMLTimer)
                        (xpList xpXMLExceptionHandler)


xpXMLStartState :: PU XMLStartState
xpXMLStartState =   xpElem "start-state" $
                    xpWrap (
                            uncurry6 XMLStartState,
                            \ t ->  (
                                        ssName t,
                                        ssSwimLane t,
                                        ssTask t,
                                        ssTransitions t,
                                        ssEvents t,
                                        ssExceptionHandlers t
                                    )
                           ) $
                    xp6Tuple    (xpOption $ xpAttr "name" xpText0)
                                (xpOption $ xpAttr "swimlane" xpText0)
                                (xpOption xpXMLTask)
                                (xpList xpXMLTransition)
                                (xpList xpXMLEvent)
                                (xpList xpXMLExceptionHandler)


xpXMLTransition :: PU XMLTransition
xpXMLTransition =   xpElem "transition" $
                    xpWrap (
                            uncurry7 XMLTransition,
                            \ t ->  (
                                        trName t,
                                        trTo t,
                                        trActions t,
                                        trScripts t,
                                        trCreateTimers t,
                                        trCancelTimers t,
                                        trExceptionHandlers t
                                    )
                           ) $
                    xp7Tuple    (xpOption $ xpAttr "name" xpText0)
                                (xpOption $ xpAttr "to" xpText0)
                                (xpList xpXMLAction)
                                (xpList xpXMLScript)
                                (xpList xpXMLCreateTimer)
                                (xpList xpXMLCancelTimer)
                                (xpList xpXMLExceptionHandler)


xpXMLState :: PU XMLState
xpXMLState =   xpElem "state" $
                    xpWrap (
                            uncurry9 XMLState,
                            \ t ->  (
                                        stName t,
                                        stAction t,
                                        stScript t,
                                        stCreateTimer t,
                                        stCancelTimer t,
                                        stTransitions t,
                                        stEvents t,
                                        stTimers t,
                                        stExceptionHandlers t
                                    )
                           ) $
                    xp9Tuple    (xpOption $ xpAttr "name" xpText0)
                                (xpOption xpXMLAction)
                                (xpOption xpXMLScript)
                                (xpOption xpXMLCreateTimer)
                                (xpOption xpXMLCancelTimer)
                                (xpList xpXMLTransition)
                                (xpList xpXMLEvent)
                                (xpList xpXMLTimer)
                                (xpList xpXMLExceptionHandler)


xpXMLTaskNode:: PU XMLTaskNode
xpXMLTaskNode =   xpElem "task-node" $
                    xpWrap (
                            uncurry9 XMLTaskNode,
                            \ t ->  (
                                        tnName t,
                                        tnSignal t,
                                        tnCreateTasks t,
                                        tnEndTasks t,
                                        tnTasks t,
                                        tnTransitions t,
                                        tnEvents t,
                                        tnTimers t,
                                        tnExceptionHandlers t
                                    )
                           ) $
                    xp9Tuple    (xpOption $ xpAttr "name" xpText0)
                                (xpOption $ xpAttr "signal" xpText0)
                                (xpOption $ xpAttr "create-tasks" xpText0)
                                (xpOption $ xpAttr "end-tasks" xpText0)
                                (xpList xpXMLTask)
                                (xpList xpXMLTransition)
                                (xpList xpXMLEvent)
                                (xpList xpXMLTimer)
                                (xpList xpXMLExceptionHandler)

xpXMLHandler :: PU XMLHandler
xpXMLHandler =  xpElem "handler" $
                    xpWrap (
                             uncurry XMLHandler ,
                             \ t -> (
                                     hClass t,
                                     hConfigType t
                                    )
                            ) $
                    xpPair  (xpAttr "class" xpText0)
                            (xpOption $ xpAttr "config-type" xpText0)

xpXMLSubProcess :: PU XMLSubProcess
xpXMLSubProcess =  xpElem "sub-process" $
                    xpWrap (
                             uncurry XMLSubProcess ,
                             \ t -> (
                                     sbName t,
                                     sbVersion t
                                    )
                            ) $
                    xpPair  (xpAttr "name" xpText0)
                            (xpOption $ xpAttr "version" xpPrim)

xpXMLEndState :: PU XMLEndState
xpXMLEndState =  xpElem "end-state" $
                    xpWrap (
                             uncurry3 XMLEndState ,
                             \ t -> (
                                     esName t,
                                     esEvents t,
                                     esExceptionHandlers t
                                    )
                            ) $
                    xpTriple    (xpOption $ xpAttr "name" xpText0)
                                (xpList xpXMLEvent)
                                (xpList xpXMLExceptionHandler)

xpXMLDecision :: PU XMLDecision
xpXMLDecision =  xpElem "decision" $
                    xpWrap (
                             uncurry6 XMLDecision ,
                             \ t -> (
                                        dName t,
                                        dHandler t,
                                        dTransitions t,
                                        dEvents t,
                                        dTimers t,
                                        dExceptionHandlers t
                                    )
                            ) $
                    xp6Tuple    (xpAttr "name" xpText0)
                                (xpOption $ xpXMLHandler)
                                (xpList xpXMLTransition)
                                (xpList xpXMLEvent)
                                (xpList xpXMLTimer)
                                (xpList xpXMLExceptionHandler)


xpXMLFork :: PU XMLFork
xpXMLFork =  xpElem "fork" $
                    xpWrap (
                             uncurry6 XMLFork ,
                             \ t -> (
                                        fName t,
                                        fScript t,
                                        fTransitions t,
                                        fEvents t,
                                        fTimers t,
                                        fExceptionHandlers t
                                    )
                            ) $
                    xp6Tuple    (xpOption $ xpAttr "name" xpText0)
                                (xpOption $ xpXMLScript)
                                (xpList xpXMLTransition)
                                (xpList xpXMLEvent)
                                (xpList xpXMLTimer)
                                (xpList xpXMLExceptionHandler)

xpXMLJoin :: PU XMLJoin
xpXMLJoin =  xpElem "join" $
                    xpWrap (
                             uncurry5 XMLJoin ,
                             \ t -> (
                                        jName t,
                                        jTransitions t,
                                        jEvents t,
                                        jTimers t,
                                        jExceptionHandlers t
                                    )
                            ) $
                    xp5Tuple    (xpAttr "name" xpText0)
                                (xpList xpXMLTransition)
                                (xpList xpXMLEvent)
                                (xpList xpXMLTimer)
                                (xpList xpXMLExceptionHandler)


xpXMLNode :: PU XMLNode
xpXMLNode =  xpElem "node" $
                    xpWrap (
                             uncurry9 XMLNode ,
                             \ t -> (
                                        nName t,
                                        nAction t,
                                        nScript t,
                                        nCreateTimer t,
                                        nCancelTimer t,
                                        nTransitions t,
                                        nEvents t,
                                        nTimers t,
                                        nExceptionHandlers t
                                    )
                            ) $
                    xp9Tuple    (xpAttr "name" xpText0)
                                (xpOption $ xpXMLAction)
                                (xpOption $ xpXMLScript)
                                (xpOption $ xpXMLCreateTimer)
                                (xpOption $ xpXMLCancelTimer)
                                (xpList xpXMLTransition)
                                (xpList xpXMLEvent)
                                (xpList xpXMLTimer)
                                (xpList xpXMLExceptionHandler)

xpXMLProcessState :: PU XMLProcessState
xpXMLProcessState =  xpElem "process-state" $
                    xpWrap (
                             uncurry7 XMLProcessState ,
                             \ t -> (
                                        psName t,
                                        psSubProcess t,
                                        psVariabes t,
                                        psTransitions t,
                                        psEvents t,
                                        psTimers t,
                                        psExceptionHandlers t
                                    )
                            ) $
                    xp7Tuple    (xpAttr "name" xpText0)
                                (xpOption $ xpXMLSubProcess)
                                (xpList xpXMLVariable)
                                (xpList xpXMLTransition)
                                (xpList xpXMLEvent)
                                (xpList xpXMLTimer)
                                (xpList xpXMLExceptionHandler)


xpXMLSuperState :: PU XMLSuperState
xpXMLSuperState =  xpElem "super-state" $
                    xpWrap (
                             uncurry14 XMLSuperState ,
                             \ t -> (
                                        spName t,
                                        spStates t,
                                        spTaskNodes t,
                                        spSuperStates t,
                                        spProcessStates t,
                                        spNodes t,
                                        spForks t,
                                        spJoins t,
                                        spDecicision t,
                                        spEndStates t,
                                        spTransitions t,
                                        spEvents t,
                                        spTimers t,
                                        spExceptionHandlers t
                                    )
                            ) $
                    xp14Tuple   (xpOption $ xpAttr "name" xpText0)
                                (xpList xpXMLState)
                                (xpList xpXMLTaskNode)
                                (xpList xpXMLSuperState)
                                (xpList xpXMLProcessState)
                                (xpList xpXMLNode)
                                (xpList xpXMLFork)
                                (xpList xpXMLJoin)
                                (xpList xpXMLDecision)
                                (xpList xpXMLEndState)
                                (xpList xpXMLTransition)
                                (xpList xpXMLEvent)
                                (xpList xpXMLTimer)
                                (xpList xpXMLExceptionHandler)
