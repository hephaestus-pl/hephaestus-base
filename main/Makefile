SPL_SRC_DIR=../spl/src
UTIL_SRC_DIR=../util/src
FM_SRC_DIR=../feature-modeling/src/
ENS_SRC_DIR=../asset-base/ensemble/src/
UC_SRC_DIR=../asset-base/uc-model/src
REQ_SRC_DIR=../asset-base/req-model/src
CMP_SRC_DIR=../asset-base/component-model/src
BPM_SRC_DIR=../asset-base/bpmn/src

SRC_DIR=src/:$(SPL_SRC_DIR):$(UTIL_SRC_DIR):$(FM_SRC_DIR):$(ENS_SRC_DIR):$(UC_SRC_DIR):$(REQ_SRC_DIR):$(CMP_SRC_DIR):$(BPM_SRC_DIR)

options=-XDeriveDataTypeable -XMultiParamTypeClasses -XRank2Types

build:
	ghc -i$(SRC_DIR) src/Main ${options}


build-code-only:
	ghc -i$(SRC_DIR) src/VMCode.hs ${options}
