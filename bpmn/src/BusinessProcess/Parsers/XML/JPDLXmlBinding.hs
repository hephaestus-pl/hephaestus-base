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
    slAssignments :: [Maybe XMLAssignment]
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
    sVariables :: [Maybe XMLVariable]
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
    eActions :: [Maybe XMLAction],
    eScripts :: [Maybe XMLScript],
    eCreateTimer :: Maybe XMLCreateTimer,
    eCancelTimer :: Maybe XMLCancelTimer
} deriving (Eq, Show)

data XMLTimer = XMLTimer {
    tiName :: Maybe String,
    tiDueDate :: String,  {-Obrigatorio-}
    tiRepeat :: Maybe String,
    tiTransition :: Maybe String,
    tiCancelEvent :: Maybe String,
    tiAction :: Maybe XMLAction,
    tiScript :: Maybe XMLScript
} deriving (Eq, Show)

data XMLExceptionHandler = XMLExceptionHandler {
    ehExceptionClass :: Maybe String,
    ehActions :: [Maybe XMLAction],
    ehScripts :: [Maybe XMLScript]
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
    tEvents :: [Maybe XMLEvent],
    tTimers :: [Maybe XMLTimer],
    tExceptionHandlers :: [Maybe XMLExceptionHandler]
} deriving (Eq, Show)

data XMLStartState = XMLStartState {
    ssName :: Maybe String,
    ssSwimLane :: Maybe String,
    ssTask :: Maybe XMLTask
} deriving (Eq, Show)

data XMLTransition = XMLTransition {
    trName :: Maybe String,
    trTo :: Maybe String,
    trActions :: [Maybe XMLAction],
    trScripts :: [Maybe XMLScript],
    trCreateTimers :: [Maybe XMLCreateTimer],
    trCancelTimers :: [Maybe XMLCancelTimer],
    trExceptionHandlers :: [Maybe XMLExceptionHandler]
} deriving (Eq, Show)

data XMLState = XMLState {
    stName :: Maybe String,
    stAction :: Maybe XMLAction,
    stScript :: Maybe XMLScript,
    stCreateTimer :: Maybe XMLCreateTimer,
    stCancelTimer :: Maybe XMLCancelTimer,
    stTransitions :: [Maybe XMLTransition],
    stEvents :: [Maybe XMLEvent],
    stTimers :: [Maybe XMLTimer],
    stExceptionHandlers :: [Maybe XMLExceptionHandler]
} deriving (Eq, Show)

data XMLTaskNode = XMLTaskNode {
    tnName :: Maybe String,
    tnSignal :: Maybe String,
    tnCreateTasks :: Maybe Bool,
    tnEndTasks :: Maybe Bool,
    tnTasks :: [Maybe XMLTask],
    tnTransitions :: [Maybe XMLTransition],
    tnEvents :: [Maybe XMLEvent],
    tnTimers :: [Maybe XMLTimer],
    tnExceptionHandlers :: [Maybe XMLExceptionHandler]
} deriving (Eq, Show)

data XMLHandler = XMLHandler {
    hClass :: String,{-obrigatorio-}
    hConfigType :: Maybe String
} deriving (Eq, Show)

data XMLDecision = XMLDecision {
    dName :: String,{-Obrigatorio-}
    dHandler :: Maybe XMLHandler,
    dTransitions :: [Maybe XMLTransition],
    dEvents :: [Maybe XMLEvent],
    dTimers :: [Maybe XMLTimer],
    dExceptionHandlers :: [Maybe XMLExceptionHandler]
} deriving (Eq, Show)

data XMLEndState = XMLEndState {
    esName :: Maybe String,
    esEvents :: [Maybe XMLEvent],
    esExceptionHandlers :: [Maybe XMLExceptionHandler]
} deriving (Eq, Show)

data XMLFork = XMLFork {
    fName :: Maybe String,
    fScript :: Maybe XMLScript,
    fTransitions :: [Maybe XMLTransition],
    fEvents :: [Maybe XMLEvent],
    fTimers :: [Maybe XMLTimer],
    fExceptionHandlers :: [Maybe XMLExceptionHandler]
} deriving (Eq, Show)

data XMLJoin = XMLJoin {
    jName :: String,{-Obrigatorio-}
    jTransitions :: [Maybe XMLTransition],
    jEvents :: [Maybe XMLEvent],
    jTimers :: [Maybe XMLTimer],
    jExceptionHandlers :: [Maybe XMLExceptionHandler]
} deriving (Eq, Show)

data XMLNode = XMLNode {
    nName :: String, {-Obrigatorio-}
    nAction :: Maybe XMLAction,
    nScript :: Maybe XMLScript,
    nCreateTimer :: Maybe XMLCreateTimer,
    nCancelTimer :: Maybe XMLCancelTimer,
    nTransitions :: [Maybe XMLTransition],
    nEvents :: [Maybe XMLEvent],
    nTimers :: [Maybe XMLTimer],
    nExceptionHandlers :: [Maybe XMLExceptionHandler]
} deriving (Eq, Show)

data XMLSubProcess = XMLSubProcess {
    sbName :: String,{-Obrigatorio-}
    sbVersion :: Maybe Integer
} deriving (Eq, Show)

data XMLProcessState = XMLProcessState {
    psName :: String,{-Obrigatorio-}
    psSubProcess :: Maybe XMLSubProcess,
    psVariabes :: [Maybe XMLVariable],
    psTransitions :: [Maybe XMLTransition],
    psEvents :: [Maybe XMLEvent],
    psTimers :: [Maybe XMLTimer],
    psExceptionHandlers :: [Maybe XMLExceptionHandler]
} deriving (Eq, Show)

data XMLSuperState = XMLSuperState {
    spName :: Maybe String,
    spStates :: [Maybe XMLState],
    spTaskNodes :: [Maybe XMLTaskNode],
    spSuperStates :: [Maybe XMLSuperState],
    spProcessStates :: [Maybe XMLProcessState],
    spNodes :: [Maybe XMLNode],
    spForks :: [Maybe XMLFork],
    spJoins :: [Maybe XMLJoin],
    spDecicision :: [Maybe XMLDecision],
    spEndStates :: [Maybe XMLEndState],
    spTransitions :: [Maybe XMLTransition],
    spEvents :: [Maybe XMLEvent],
    spTimers :: [Maybe XMLTimer],
    spExceptionHandlers :: [Maybe XMLExceptionHandler]
} deriving (Eq, Show)

data XMLProcessDefinition = XMLProcessDefinition {
    pdName :: Maybe String,
    pdSwimlanes :: [Maybe XMLSwimLane]--,
    --pdStartState :: XMLStartState
} deriving (Eq, Show)


{------------Picklers ------------------}

instance XmlPickler XMLProcessDefinition where
    xpickle = xpXMLProcessDefinition

instance XmlPickler XMLSwimLane where
    xpickle = xpXMLSwimLane

instance XmlPickler XMLAssignment where
    xpickle = xpXMLAssignment

xpXMLProcessDefinition :: PU XMLProcessDefinition

xpXMLProcessDefinition = xpElem "process-definition" $
                         xpWrap (uncurry XMLProcessDefinition,
                                \ t -> ( pdName t,
                                         pdSwimlanes t)) $
                         {-tupla de 7 no final-}
                         xpPair (xpOption ( xpAttr "name" xpText0))
                                xpickle

xpXMLSwimLane :: PU XMLSwimLane
xpXMLSwimLane = xpElem "swimlane" $
                xpWrap (uncurry XMLSwimLane,
                        \ t -> (slName t,
                                slAssignments t)) $
                xpPair (xpOption (xpAttr "name" xpText0))
                       (xpList xpickle)


xpXMLAssignment	:: PU XMLAssignment
xpXMLAssignment = xpElem "assignment" $
                  xpWrap ( \((c,ct,e)) -> XMLAssignment c ct e,
                           \ t -> (aClass t, aConfig_type t, aExpression t))  $
                  xpTriple (xpOption ( xpAttr "class" xpText0))
                           (xpOption( xpAttr "config-type" xpText0))
                           (xpOption( xpAttr "expression" xpText0))


