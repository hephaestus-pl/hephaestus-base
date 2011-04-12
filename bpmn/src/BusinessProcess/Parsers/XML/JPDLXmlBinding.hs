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
    ssTask :: Maybe XMLTask
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
    tnCreateTasks :: Maybe Bool,
    tnEndTasks :: Maybe Bool,
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
    pdName :: Maybe String,
    pdActions :: [XMLAction],
    pdSwimlanes :: [XMLSwimLane]{-,
    pdStartState :: XMLStartState,
    pdStates :: [XMLState],
    pdTaskNodes :: [XMLTaskNode],
    pdSuperStates :: [XMLSuperState],
    pdProcessStates :: [XMLProcessState],
    pdNodes :: [XMLNode],
    pdForks :: [XMLFork],
    pdJoins :: [XMLJoin],
    pdDecicision :: [XMLDecision],
    pdEndStates :: [XMLEndState],
    pdScripts :: [XMLScript],
    pdCreateTimers :: [XMLCreateTimer],
    pdCancelTimers :: [XMLCancelTimer],
    pdTasks :: [XMLTask],
    pdEvents :: [XMLEvent],
    pdExceptionHandlers :: [XMLExceptionHandler]
    -}
} deriving (Eq, Show)

{- Utilities -}

uncurry19 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o -> p -> q -> r -> s -> t) -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) -> t
uncurry19 u ~(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)	= u a b c d e f g h i j k l m n o p q r s

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 fn (a, b, c, d, e) = fn a b c d e

uncurry6 :: (a -> b -> c -> d -> e -> f -> g) -> (a, b, c, d, e, f) -> g
uncurry6 fn (a, b, c, d, e, f) = fn a b c d e f

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


xpXMLProcessDefinition :: PU XMLProcessDefinition

xpXMLProcessDefinition = xpElem "process-definition" $
                         xpWrap (
                                    uncurry3 XMLProcessDefinition,
                                    \ t -> (
                                            pdName t,
                                            pdActions t ,
                                            pdSwimlanes t{-,
                                            pdStartState t,
                                            pdStates t,
                                            pdTaskNodes t,
                                            pdSuperStates t,
                                            pdProcessStates t,
                                            pdNodes t,
                                            pdForks t,
                                            pdJoins t,
                                            pdDecicision t,
                                            pdEndStates t,
                                            pdScripts t,
                                            pdCreateTimers t,
                                            pdCancelTimers t,
                                            pdTasks t,
                                            pdEvents t,
                                            pdExceptionHandlers t-}
                                            )
                                 ) $
                         xpTriple   (xpOption $ xpAttr "name" xpText0)
                                    (xpList  xpXMLAction)
                                    (xpList  xpXMLSwimLane)


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
