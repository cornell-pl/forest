/* ********************* BEGIN_MACROS(pglx-codegen-macros-gen.h) ********************** */
/*
 * Macros used to generate pads-galax code.
 * 
 * Yitzhak Mandelbaum
 * AT&T Labs Research
 */

/* ********************************** END_HEADER ********************************** */

/* ================================================================================ */

#define GLX_STR_MATCH(p,s) (strcmp((p),(s)) == 0)

#define NODE_NEW_BODY(ty)
  PDCI_node_t *result;
  PDCI_MK_NODE (result,& ty ## _node_vtable,
		parent,name,m,pd,rep,kind, PDCI_MacroArg2String(ty) "_node_new")
/* END_MACRO */

#define NODE_NEW_RET()
result
/* END_MACRO */

#define CACHED_NODE_INIT_BODY(ty,NUM_CHILDREN)
  /* Setup the virtual table */
  self->vt = & ty ## _cachedNode_vtable;
  
  /* Setup node-type specific fields */
  self->child_cache = (PDCI_node_t **)PDCI_NEW_LIST(NUM_CHILDREN);
  if(self->child_cache == NULL)
    error (ERROR_FATAL, "ALLOC_ERROR: in " PDCI_MacroArg2String(ty) "_cachedNode_init")
/* END_MACRO */

#define CACHED_NODE_INIT_RET()
self
/* END_MACRO */

#define SND_NODE_INIT_BODY(ty)
   /* Setup the virtual table */              
  self->vt = & ty ## _sndNode_vtable;
					      
  /* Setup node-type specific fields  */      
  self->manager = manager;		      
  self->ancestor_idx = ancestor_idx;
  self->ptr_gen = gen;
  self->idx = idx
/* END_MACRO */

#define SND_NODE_INIT_RET()
self
/* END_MACRO */

/* node kthChild function for structured types (i.e. have structured_pd's). */
#define STR_NODE_KTH_CHILD_BODY_BEGIN(ty)
  PDCI_node_t *result = 0;
  ty *rep=(ty *) (self->rep);
  ty ## _pd *pd=(ty ## _pd *) (self->pd);
  ty ## _m *m=(ty ## _m *) (self->m);

  switch(idx){
    /* parse descriptor child */
  case 0:
    result = PDCI_structured_pd_node_new(self,"pd",pd,PDCI_MacroArg2String(ty) "_node_kthChild");
    break
/* END_MACRO */

#define STR_NODE_KTH_CHILD_BODY_END()
  }
/* END_MACRO */

#define STR_NODE_KTH_CHILD_RET()
result
/* END_MACRO */

/* case for kthChild function, where child is a computed field */
#define NODE_KC_CASE_COMP(ty,fieldNumIN,fieldTy,fieldNameIN)
  case fieldNumIN:
    result = fieldTy ## _node_new(self,PDCI_MacroArg2String(fieldNameIN),
				  0,0,&(rep->fieldNameIN),
				  "element", PDCI_MacroArg2String(ty) "_node_kthChild");
    break
/* END_MACRO */

/* case for kthChild function */
#define NODE_KC_CASE(ty,fieldNumIN,fieldTy,fieldNameIN)
  case fieldNumIN:
    result = fieldTy ## _node_new(self,PDCI_MacroArg2String(fieldNameIN),
				  &(m->fieldNameIN),
				  &(pd->fieldNameIN),
				  &(rep->fieldNameIN),
				  "element", PDCI_MacroArg2String(ty) "_node_kthChild");
    break
/* END_MACRO */

#define STR_NODE_KTH_CHILD_NAMED_BODY(ty,NAMES...)
  PDCI_node_t *result = 0;
  PDCI_childIndex_t i;
  const char *fieldNames[] = {NAMES,0}; 
  /* The index must be 0 as all field names are unique.*/
  if (idx != 0)
    return result;
  for (i = 0; 1; i++) {
    if (fieldNames[i] == 0)
      return 0;
    if (GLX_STR_MATCH(name, fieldNames[i]))
      break;
  } 
  /* fall through if i set correctly */
/* END_MACRO */

#define STR_NODE_KTH_CHILD_NAMED_RET()
(self->vt->kth_child)(self,i)
/* END_MACRO */

#define CACHED_NODE_KTH_CHILD_BODY(ty,NUM_CHILDREN)
  PDCI_node_t *result = 0;

  /* Array bounds check for cache.*/
  if (idx >= NUM_CHILDREN) /* non-existent child */
    return result;

  result = self->child_cache[idx];
  if (result == NULL){
    /* create a new node for the kth child */
    result = ty ## _node_kthChild(self,idx);

    /*  initialize the node to be a cachedNode. */
    (result->vt->cachedNode_init)(result);

    /* cache the result */
    self->child_cache[idx] = result;
  }

/* END_MACRO */

#define CACHED_NODE_KTH_CHILD_RET()
PDCI_ALIAS_NODE(result)
/* END_MACRO */

#define STR_SND_NODE_KTH_CHILD_BODY_BEGIN(ty)
  PDCI_node_t *result = 0;
  ty *rep;
  ty ## _pd *pd;
  ty ## _m *m=(ty ## _m *) (self->m);

  /* Make sure that the node is valid before attempting to access its contents. */ 
  PDCI_sndNode_validate(self);
  rep = (ty *) (self->rep);
  pd = (ty ## _pd *) (self->pd);

  switch(idx){
  case 0: 
    /* parse descriptor child */
    result = PDCI_structured_pd_node_new(self,"pd",pd,PDCI_MacroArg2String(ty) "_sndNode_kthChild");
    PDCI_structured_pd_sndNode_init(result,self->manager,self->ancestor_idx,self->ptr_gen,idx);
    break
/* END_MACRO */

#define STR_SND_NODE_KTH_CHILD_BODY_END()
  }

/* END_MACRO */  

#define STR_SND_NODE_KTH_CHILD_RET()
result
/* END_MACRO */

#define SND_NODE_KC_CASE(ty,fieldNumIN,fieldTy,fieldNameIN)
  case fieldNumIN:
    result = fieldTy ## _node_new(self,PDCI_MacroArg2String(fieldNameIN),
				  &(m->fieldNameIN),
				  &(pd->fieldNameIN),
				  &(rep->fieldNameIN),
				  "element", PDCI_MacroArg2String(ty) "_sndNode_kthChild");
    fieldTy ## _sndNode_init(result,self->manager,self->ancestor_idx,self->ptr_gen,idx);
    break
/* END_MACRO */

#define SND_NODE_KC_CASE_COMP(ty,fieldNumIN,fieldTy,fieldNameIN)
  case fieldNumIN:
    result = fieldTy ## _node_new(self,PDCI_MacroArg2String(fieldNameIN),
                                  0,0,&(rep->fieldNameIN),
				  "element", PDCI_MacroArg2String(ty) "_sndNode_kthChild");
    fieldTy ## _sndNode_init(result,self->manager,self->ancestor_idx,self->ptr_gen,idx);
    break
/* END_MACRO */

#define STR_NODE_PATH_WALK_BODY_BEGIN()
  Perror_t res = P_ERR;
  PDCI_childIndex_t idx;
  
  if (path.length > 0){
    /* modifies path */
    idx = PDCI_PATH_GET(path);

    switch(idx){
    case 0: 
      *pd_out = NULL;
      *m_out = NULL;
      res = PDCI_structured_pd_node_pathWalk(pads,(PDCI_structured_pd *)pd,path,rep_out);
      break
/* END_MACRO */

#define STR_NODE_PATH_WALK_BODY_END()
    }
  }else{
    *rep_out = rep;
    *pd_out = pd;
    *m_out = m;

    res = P_OK;
  }
/* END MACRO */

#define STR_NODE_PATH_WALK_RET()
res
/* END_MACRO */

#define NODE_PW_CASE(fieldNumIN,fieldTy,fieldNameIN)
    case fieldNumIN:
      res = fieldTy ## _node_pathWalk(pads,&(m->fieldNameIN),&(pd->fieldNameIN),&(rep->fieldNameIN),path,m_out,pd_out,rep_out);      
      break
/* END_MACRO */

#define NODE_PW_CASE_COMP(fieldNumIN,fieldTy,fieldNameIN)
    case fieldNumIN:
      res = fieldTy ## _node_pathWalk(pads,0,0,&(rep->fieldNameIN),path,m_out,pd_out,rep_out);      
     /* ASSERT(*m_out == 0 && *pd_out == 0); */
      break
/* END_MACRO */

#define ARR_LENGTH(ty) (2 + ((ty *)self->rep)->length)

#define ARR_NODE_KTH_CHILD_BODY(ty,childTy)
  PDCI_node_t *result = 0;
  ty *rep=(ty *) (self->rep);
  ty ## _pd *pd=(ty ## _pd *) (self->pd);
  ty ## _m *m=(ty ## _m *) (self->m);

  switch(idx){
  case 0: /* parse descriptor child */
    result = PDCI_sequenced_pd_node_new(self,"pd",pd, PDCI_MacroArg2String(ty) "_node_kthChild");
    break;
  case 1: /* length field */
    result = Puint32_val_node_new(self,"length",&(rep->length),PDCI_MacroArg2String(ty) "_node_kthChild");
    break;
  default: /* now do elements */
    idx -= 2;
    if (idx < rep->length){
      result = childTy ## _node_new(self,"elt",&(m->element),&(pd->elts)[idx],&(rep->elts)[idx],"element",
				    PDCI_MacroArg2String(ty)"_node_kthChild");
    }
    break;
  }
/* END_MACRO */

#define ARR_NODE_KTH_CHILD_RET()
result
/* END_MACRO */

#define ARR_NODE_KTH_CHILD_NAMED_BODY(ty)
  ty *rep=(ty *) (self->rep);
  PDCI_node_t *result = 0;
  PDCI_childIndex_t k = 0;

  if (GLX_STR_MATCH(name,"pd")){
    if(idx == 0) k = idx;
    else         return result;
  }else if(GLX_STR_MATCH(name,"length")){
    if(idx == 0) k = 1;
    else         return result;
  }else if(GLX_STR_MATCH(name,"elt") && idx < rep->length)  
    k = idx + 2;
  else return result;

/* END_MACRO */

#define ARR_NODE_KTH_CHILD_NAMED_RET()
(self->vt->kth_child)(self,k);
/* END_MACRO */

#define ARR_SND_NODE_KTH_CHILD_BODY(ty,childTy)
  PDCI_node_t *result = 0;
  ty *rep;
  ty ## _pd *pd;
  ty ## _m *m=(ty ## _m *) (self->m);

  /* Make sure that the node is valid before attempting to access its contents. */ 
  PDCI_sndNode_validate(self);
  rep = (ty *) (self->rep);
  pd = (ty ## _pd *) (self->pd);

  switch(idx){
  case 0: 
    /* parse descriptor child */
    result = PDCI_sequenced_pd_node_new(self,"pd",pd,PDCI_MacroArg2String(ty) "_sndNode_kthChild");
    PDCI_sequenced_pd_sndNode_init(result,self->manager,self->ancestor_idx,self->ptr_gen,idx);
    break;
  case 1: /* length field */
    result = Puint32_val_node_new(self,"length",&(rep->length),PDCI_MacroArg2String(ty) "_sndNode_kthChild");
    Puint32_val_sndNode_init(result,self->manager,self->ancestor_idx,self->ptr_gen,idx);
    break;
  default: /* now do elements */
    idx -= 2;
    if (idx < rep->length){
      result = childTy ## _node_new(self,"elt",&(m->element),&(pd->elts)[idx],&(rep->elts)[idx],"element",
				       PDCI_MacroArg2String(ty)"_sndNode_kthChild");
      childTy ## _sndNode_init(result,self->manager,self->ancestor_idx,self->ptr_gen,idx + 2);
    }
    break;
  }
/* END_MACRO */

#define ARR_SND_NODE_KTH_CHILD_RET()
result
/* END_MACRO */

#define ARR_NODE_PATH_WALK_BODY(childTy)
  Perror_t res = P_ERR;
  PDCI_childIndex_t idx;
  
  if (path.length > 0){
    /* modifies path */
    idx = PDCI_PATH_GET(path);

    switch(idx){
    case 0: 
      *pd_out = NULL;
      *m_out = NULL;
      res = PDCI_structured_pd_node_pathWalk(pads,(PDCI_structured_pd *)pd,path,rep_out);
      break;
    case 1:
      *pd_out = NULL;
      *m_out = NULL;
      res = Puint32_val_node_pathWalk(pads,&(rep->length),path,rep_out);      
      break;
    default:
      idx -= 2;
      if (idx < rep->length){
	res = childTy ## _node_pathWalk(pads,&(m->element),&(pd->elts)[idx],&(rep->elts)[idx],
					path,m_out,pd_out,rep_out);
      }
      break;
    }
  }else{
    *rep_out = rep;
    *pd_out = pd;
    *m_out = m;

    res = P_OK;
  }
/* END MACRO */

#define ARR_NODE_PATH_WALK_RET()
res
/* END_MACRO */


/* ********************************* BEGIN_TRAILER ******************************** */
/* ********************************** END_MACROS ********************************** */
