
/* XXX TEMP XXX */
Sfio_t *P_io_get(P_t *pads)
{
  return pads->io;
}

Perror_t P_io_seek(P_t *pads, Sfoff_t offset){
  Sfio_t *io = P_io_get(pads);
  P_io_close(pads);
  Sfoff_t off = sfseek(io, offset, 0);
  if (-1 == off || offset != off) {
    return P_ERR;
  }

  return P_io_set(pads, io);
}

#define SN_ELT_ALLOC_FROM_RBUF(ty,eltTy, eltPdTy)
Perror_t ty ## _smartNode_eltAlloc(PDCI_node_t *smartNode, P_t *pads,
				       void **elt_pd, void **elt_rep)
{
  SN_ELT_ALLOC_FROM_RBUF_BODY(ty,eltTy,eltPdTy,
			      smartNode,pads,elt_pd,elt_rep);
  return SN_ELT_ALLOC_FROM_RBUF_RET();
}
/* END_MACRO */

#define SN_ELT_ALLOC(ty,eltTy, eltPdTy)
Perror_t ty ## _smartNode_eltAlloc(PDCI_node_t *smartNode, P_t *pads,
				       void **elt_pd, void **elt_rep)
{
  SN_ELT_ALLOC_BODY(ty,eltTy, eltPdTy,
		    smartNode,pads,elt_pd,elt_rep);
  return SN_ELT_ALLOC_RET();
}
/* END_MACRO */

#define SN_SEQ_ELT_READ(ty,eltTy,eltPdTy,ST_PARAMS,...)
Pread_res_t ty ## _seqSmartNode_eltRead(PDCI_node_t *smartNode, P_t *pads, PDCI_smart_elt_info_t *info)
{
  SN_SEQ_ELT_READ_BODY(ty,eltTy,eltPdTy,
		       smartNode,pads,info,ST_PARAMS,__VA_ARGS__)
  return SN_SEQ_ELT_READ_RET();
}
/* END_MACRO */

#define SN_SEQ_ELT_FREE(ty)
Perror_t ty ## _seqSmartNode_eltFree(P_t *pads, PDCI_smart_elt_info_t *info){
  SN_SEQ_ELT_FREE_BODY(ty,pads,info);
  return SN_SEQ_ELT_FREE_RET();
}
/* END_MACRO */

#define SN_ELT_PATH_WALK(ty,eltTy,eltPdTy,eltMaskTy)
Perror_t ty ## _smartNode_eltPathWalk(P_t *pads, void *m, void *pd, void *rep, PDCI_path_t path,
					     void **m_out, void **pd_out, void **rep_out)
{
  SN_ELT_PATH_WALK_BODY(eltTy,eltPdTy,eltMaskTy);
  return SN_ELT_PATH_WALK_RET(pads,path,m_out,pd_out,rep_out);
}
/* END_MACRO */

#define SN_ARRAY_INFO_INIT(ty,initFunIN)
ty ## _array_info_t *ty ## _array_info_init(P_t *pads,unsigned int max_elts)
{
  SN_ARRAY_INFO_INIT_BODY(ty,pads,max_elts,initFunIN);
  return SN_ARRAY_INFO_INIT_RET();
}
/* END_MACRO */

#define SN_SEQ_INIT(ty,ST_PARAMS,...)
PDCI_node_t *ty ## _seqSmartNode_init(PDCI_node_t *self, unsigned int max_elts)
{
  SN_SEQ_INIT_BODY(ty,self,max_elts,ST_PARAMS,__VA_ARGS__);
  return SN_SEQ_INIT_RET();
}
/* END_MACRO */

#define SN_KTH_CHILD(ty,eltTy)
PDCI_node_t * ty ## _smartNode_kthChild (PDCI_node_t *self, PDCI_childIndex_t idx)
{
  SN_KTH_CHILD_BODY(ty,eltTy,self,idx);
  return SN_KTH_CHILD_RET();
}
/* END_MACRO */

#define SN_SEQ_VT(ty)
PDCI_vtable_t const ty ## _seqSmartNode_vtable = {PDCI_error_cachedNode_init,
						    ty ## _smartNode_kthChild,
						    ty ## _node_kthChildNamed,
						    PDCI_cachedNode_free,
						    PDCI_smartNode_getId,
						    PDCI_error_typed_value,
						    0}
/* END_MACRO */

SN_ELT_ALLOC(barArray,bar, bar_pd)
SN_SEQ_ELT_READ(barArray, bar, bar_pd, SN_WRAP_PARAMS())
SN_SEQ_ELT_FREE(barArray)
SN_ELT_PATH_WALK(barArray, bar, bar_pd, bar_m)
SN_ARRAY_INFO_INIT(barArray,SN_NO_OP_INIT)
SN_SEQ_INIT(barArray,SN_NO_ST_PARAMS)
SN_KTH_CHILD(barArray, bar)

SN_SEQ_VT(barArray);

/*

Perror_t barArray_seqSmartNode_eltFree(P_t *pads, PDCI_smart_elt_info_t *info);
 
Perror_t barArray_smartNode_eltPathWalk(P_t *pads, void *m, void *pd, void *rep, PDCI_path_t path,
					     void **m_out, void **pd_out, void **rep_out);


void P_sn_handleFailure(P_t *pads, PDCI_smart_node_t *node, 
					  PDCI_smart_elt_info_t *info,
					   int error_level,
					   const char *descr);
Pread_res_t barArray_seqSmartNode_eltRead(PDCI_node_t *smartNode, P_t *pads, PDCI_smart_elt_info_t *info);

PDCI_vtable_t const barArray_dummySmartNode_vtable={PDCI_error_cachedNode_init,
						    barArray_smartNode_kthChild,
						    barArray_node_kthChildNamed,
						    PDCI_cachedNode_free,
						    PDCI_error_typed_value,
						    0};

Perror_t barArray_dummySmartNode_eltRead(PDCI_node_t *smartNode, P_t *pads, PDCI_smart_elt_info_t *info){
  PDCI_smart_node_t *sn  = smartNode->snExt;
  PDCI_smart_array_info_t 
                    *sa  = (PDCI_smart_array_info_t *)sn->elt_state;
  barArray          *rep = (barArray *)smartNode->rep;
  barArray_pd       *pd  = (barArray_pd *)smartNode->pd;
  PDCI_childIndex_t  idx = info->idx;

  sa->tmap[idx].rep = &rep->elts[idx];
  sa->tmap[idx].pd  = &pd->elts[idx];
  
  return P_OK;
}

Perror_t barArray_dummySmartNode_eltFree(P_t *pads, PDCI_smart_elt_info_t *info){
  return P_ERR;
}
 
void barArray_dummySmartNode_handleFailure(P_t *pads, PDCI_smart_node_t *node, 
					  PDCI_smart_elt_info_t *info,
					   int error_level,
					   const char *descr)
{
  error(error_level, descr);  
}
*/
