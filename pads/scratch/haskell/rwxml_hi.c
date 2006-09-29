#define DEF_INPUT_FILE "CPUTime.hi"
#define PADS_TY(suf) Hi_t ## suf
#define IO_DISC_MK P_norec_noseek_make(0)

#define IFACE_TY_VAR  m.decls.decls.element.iFaceDecl.branches.iFaceId.ty
#define IFACE_TY_PATH branches.forAllTy.iFaceType
#define KIND_VAR      IFACE_TY_VAR->branches.forAllTy.iFaceTvBndr.kind
#define KIND_PATH_ARG    branches.funKindBody.arg
#define KIND_PATH_RESULT branches.funKindBody.result

#define CUSTOM_MASK_CODE \
do{\
PADS_TY(_m_init)(pads, &m, READ_MASK);  \
P_DynamicMaskInit(IFACE_TY_VAR, IfaceType_t_m, _IfaceType_t_m, READ_MASK,\
		  IFACE_TY_VAR-> IFACE_TY_PATH);\
P_DynamicMaskInit(KIND_VAR, Kind_t_m, _Kind_t_m, READ_MASK,KIND_VAR->KIND_PATH_ARG);\
KIND_VAR->KIND_PATH_RESULT = KIND_VAR;\
}while(0)

#define COPY_STRINGS 1
#define CUSTOM_MASK_CODE initHiMask(pads, &m, P_CheckAndSet)
#include "hi.h"
#include "template/read_orig_write_xml.h"

