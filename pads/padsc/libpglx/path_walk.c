#include "pglx-internal.h"

#define PDCI_DEF_BASE_PATH_WALK(ty)\
Perror_t ty ## _node_pathWalk(P_t *pads, Pbase_m *m, Pbase_pd *pd, ty *rep, PDCI_path_t path,\
			      void **m_out, void **pd_out, void **rep_out)\
{\
  PDCI_childIndex_t idx;\
  Perror_t res = P_ERR; \
                        \
  if (path.length > 0){ \
    /* modifies path */ \
    PDCI_PATH_REMOVE(ty,path,idx,path);\
\
    switch (idx) { \
      case 0: \
	*pd_out = NULL;\
	*m_out = NULL;\
	res = Pbase_pd_node_pathWalk(pads,pd,path,rep_out);\
	break; \
      case 1: \
	if (pd->errCode == P_NO_ERR || pd->errCode == P_USER_CONSTRAINT_VIOLATION) { \
	  *pd_out = NULL;\
	  *m_out = NULL;										    \
	  res = ty ## _val_node_pathWalk(pads,rep,path,rep_out); \
	} \
	break; \
    } \
  }else{\
    *rep_out = rep;\
    *pd_out = pd;\
    *m_out = m;\
    res = P_OK;    \
  }\
\
  return res; \
}\
\
Perror_t ty ## _val_node_pathWalk(P_t *pads, ty *rep, PDCI_path_t path,void **rep_out)\
{ \
  PDCI_childIndex_t idx;\
  Perror_t res = P_ERR; \
                        \
  if (path.length > 0){ \
    /* modifies path */ \
    PDCI_PATH_REMOVE(ty,path,idx,path);\
\
    /* the only valid idx is 0  */\
    if (idx == 0)\
      res = ty ## _text_node_pathWalk(pads,rep,path,rep_out);\
  }else{\
    *rep_out = rep;\
    res = P_OK;    \
  }\
\
  return res; \
} \
\
Perror_t ty ## _text_node_pathWalk(P_t *pads, ty *rep, PDCI_path_t path,void **rep_out)\
{ \
  Perror_t res = P_ERR; \
                        \
  if (path.length == 0){ \
    *rep_out = rep;\
    res = P_OK;    \
  }\
\
  return res; \
} 

PDCI_DEF_BASE_PATH_WALK(Pchar);
PDCI_DEF_BASE_PATH_WALK(Pstring);
PDCI_DEF_BASE_PATH_WALK(Pint8);
PDCI_DEF_BASE_PATH_WALK(Pint16);
PDCI_DEF_BASE_PATH_WALK(Pint32);
PDCI_DEF_BASE_PATH_WALK(Pint64);
PDCI_DEF_BASE_PATH_WALK(Puint8);
PDCI_DEF_BASE_PATH_WALK(Puint16);
PDCI_DEF_BASE_PATH_WALK(Puint32);
PDCI_DEF_BASE_PATH_WALK(Puint64);

Perror_t Ppos_t_node_pathWalk(P_t *pads, Ppos_t *pos, PDCI_path_t path, void **rep_out)
{
  PDCI_childIndex_t idx;
  Perror_t res = P_ERR; 
                        
  if (path.length > 0){ 
    /* modifies path */ 
    PDCI_PATH_REMOVE(Ppos_t,path,idx,path);

    switch (idx) {
    case 0:
      res = Pint32_val_node_pathWalk(pads,&(pos->byte),path,rep_out);
      break;
    case 1:
      res = Pint32_val_node_pathWalk(pads,&(pos->num),path,rep_out);
      break;
    case 2:
      /*  PDCI_MK_TNODE(result[2], &Puint64_val_vtable,  self, "offset",  (Puint64)(pos->offset), WHATFN); */
      break;
    }
  }else{
    *rep_out = pos;
    res = P_OK;    
  }

  return res; 
}

Perror_t Ploc_t_node_pathWalk(P_t *pads, Ploc_t *loc, PDCI_path_t path, void **rep_out)
{
  PDCI_childIndex_t idx;
  Perror_t res = P_ERR; 
                        
  if (path.length > 0){ 
    /* modifies path */ 
    PDCI_PATH_REMOVE(Ploc_t,path,idx,path);

    switch (idx) {
    case 0:
      res = Ppos_t_node_pathWalk(pads,&(loc->b),path,rep_out);
      break;
    case 1:
      res = Ppos_t_node_pathWalk(pads,&(loc->e),path,rep_out);
      break;
    }
  }else{
    *rep_out = loc;
    res = P_OK;    
  }

  return res; 
}

Perror_t Pbase_pd_node_pathWalk(P_t *pads, Pbase_pd *pd, PDCI_path_t path, void **rep_out)
{
  PDCI_childIndex_t idx;
  Perror_t res = P_ERR; 
                        
  if (path.length > 0){ 
    /* modifies path */ 
    PDCI_PATH_REMOVE(Pbase_pd,path,idx,path);

    switch (idx) {
    case 0:
      res = Puint32_val_node_pathWalk(pads,&(pd->pstate),path,rep_out);
      break;
    case 1:
      res = Puint32_val_node_pathWalk(pads,&(pd->errCode),path,rep_out);
      break;
    case 2:
      if (pd->errCode >= 100) {
	res = Ploc_t_node_pathWalk(pads,&(pd->loc),path,rep_out);
      }
      break;
    }
  }else{
    *rep_out = pd;
    res = P_OK;    
  }

  return res; 
}

Perror_t PDCI_structured_pd_node_pathWalk(P_t *pads, PDCI_structured_pd *pd, PDCI_path_t path, void **rep_out)
{
  PDCI_childIndex_t idx;
  Perror_t res = P_ERR; 
                        
  if (path.length > 0){ 
    /* modifies path */ 
    PDCI_PATH_REMOVE(PDCI_structured_pd,path,idx,path);

    switch (idx) {
    case 0:
      res = Puint32_val_node_pathWalk(pads,&(pd->pstate),path,rep_out);
      break;
    case 1:
      res = Puint32_val_node_pathWalk(pads,&(pd->nerr),path,rep_out);
      break;
    case 2:
      res = Puint32_val_node_pathWalk(pads,&(pd->errCode),path,rep_out);
      break;
    case 3:
      if (pd->errCode >= 100) {
	res = Ploc_t_node_pathWalk(pads,&(pd->loc),path,rep_out);
      }
      break;
    }
  }else{
    *rep_out = pd;
    res = P_OK;    
  }

  return res; 
}

Perror_t PDCI_sequenced_pd_node_pathWalk(P_t *pads, PDCI_sequenced_pd *pd, PDCI_path_t path, void **rep_out)
{
  PDCI_childIndex_t idx;
  Perror_t res = P_ERR; 
                        
  if (path.length > 0){ 
    /* modifies path */ 
    PDCI_PATH_REMOVE(PDCI_sequenced_pd,path,idx,path);

    switch (idx) {
    case 0:
      res = Puint32_val_node_pathWalk(pads,&(pd->pstate),path,rep_out);
      break;
    case 1:
      res = Puint32_val_node_pathWalk(pads,&(pd->nerr),path,rep_out);
      break;
    case 2:
      res = Puint32_val_node_pathWalk(pads,&(pd->errCode),path,rep_out);
      break;
    case 3:
      res = Puint32_val_node_pathWalk(pads,&(pd->neerr),path,rep_out);
      break;
    case 4:
      res = Puint32_val_node_pathWalk(pads,&(pd->firstError),path,rep_out);
      break;
    case 5:
      if (pd->errCode >= 100) {
	res = Ploc_t_node_pathWalk(pads,&(pd->loc),path,rep_out);
      }
      break;
    }
  }else{
    *rep_out = pd;
    res = P_OK;    
  }

  return res; 
}

Perror_t PDCI_cstr_val_node_pathWalk(P_t *pads,char *rep, PDCI_path_t path,void **rep_out)
{ 
  Perror_t res = P_ERR; 
                        
  if (path.length == 0){ 
    *rep_out = rep;
    res = P_OK;    
  }

  return res; 
} 

