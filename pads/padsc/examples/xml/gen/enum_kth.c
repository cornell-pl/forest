#define GLX_STR_MATCH(p,s) (strcmp((p),(s)) == 0)
#define GLX_ASSERT(cond,whatfn,errmsg)\
do {\
  if (!cond) {\
    failwith(whatfn ": " errmsg);\
  }\
while(0)

PDCI_node_t *bar_node_new(PDCI_node_t *parent,
			 const char *name, 
			 void* m, void* pd, void* rep,
			 const char *kind,
			 const char *whatfn){
  PDCI_node_t *result;
  PDCI_MK_NODE (result,&bar_node_vtable,
		parent,name,m,pd,rep,kind,"bar_node_new");
  return result;
}
 
PDCI_node_t *bar_cachedNode_init(PDCI_node_t *self){

  // Setup the virtual table
  self->vt = & bar_cachedNode_vtable;

  // Setup node-type specific fields
  self->child_cache = (PDCI_node_t **)PDCI_NEW_LIST(4);
  if(self->child_cache == NULL)
    failwith ("PADS/Galax ALLOC_ERROR: in bar_cachedNode_init");  

  return self;
}

PDCI_node_t *bar_sndNode_init(PDCI_node_t *self, PDCI_smart_elt_info_t *elt,  
			      PDCI_gen_t gen, PDCI_path_t path)
{
  PDCI_SND_INIT(bar,self,elt,gen,path);
  return self;
}

PDCI_node_t *bar_node_kthChild (PDCI_node_t *self, PDCI_childIndex_t idx)
{
  bar *rep=(bar *) (self->rep);
  bar_pd *pd=(bar_pd *) (self->pd);
  bar_m *m=(bar_m *) (self->m);
  PDCI_node_t *result = 0;

  switch(idx){
  case 0: 
    // parse descriptor child
    // PDCI_MK_TNODE (result, &PDCI_structured_pd_vtable,self,"pd",pd,"bar_kth_child");
    result = PDCI_structured_pd_node_new(self,"pd",pd,"bar_node_kthChild");
    break;
  case 1:
    // PDCI_MK_NODE (result,&Pint16_vtable,self,"f1",&(m->f1),&(pd->f1),&(rep->f1),"element","bar_node_kthChild");
    result = Pint16_node_new(self,"f1",&(m->f1),&(pd->f1),&(rep->f1),"element","bar_node_kthChild");
    break;
  case 2:
    // PDCI_MK_NODE (result,&Pint32_vtable,self,"f2",&(m->f2),&(pd->f2),&(rep->f2),"element","bar_node_kthChild"); 
    result = Pint32_node_new(self,"f2",&(m->f2),&(pd->f2),&(rep->f2),"element","bar_node_kthChild");
    break;
  case 3:
    // PDCI_MK_NODE (result,&Pchar_vtable,self,"f3",&(m->f3),&(pd->f3),&(rep->f3),"element","bar_node_kthChild");
    result = Pchar_node_new(self,"f3",&(m->f3),&(pd->f3),&(rep->f3),"element","bar_node_kthChild");
   break;
  }
  return result;
}

PDCI_node_t *bar_node_kthChildNamed (PDCI_node_t *self, PDCI_childIndex_t idx, const char *name)
{
  PDCI_node_t *result = 0;
  
  // The index must be 0 as all field names are unique.
  if (idx != 0)
    return result;

  if (GLX_STR_MATCH(name,"pd"))       idx = 0;
  else if (GLX_STR_MATCH(name,"f1"))  idx = 1;
  else if (GLX_STR_MATCH(name,"f2"))  idx = 2;
  else if (GLX_STR_MATCH(name,"f3"))  idx = 3;
  else return result;

  return (self->vt->kth_child)(self,idx);
}

PDCI_node_t *bar_cachedNode_kthChild (PDCI_node_t *self, PDCI_childIndex_t idx)
{
  PDCI_node_t *result = 0;

  // Array bounds check for cache.
  if (idx >= 4) // non-existent child
    return result;

  result = self->child_cache[idx];
  if (result == NULL){
    // create a new node for the kth child
    result = bar_node_kthChild(self,idx);

    // initialize the node to be a cachedNode.
    (result->vt->cachedNode_init)(result);

    // cache the result
    self->child_cache[idx] = result;
  }

  return PDCI_ALIAS_NODE(result);
}

const unsigned char bar_pathWidth = 2;
const unsigned char bar_pathMask = 0x3;
#undef WHATFN
#define WHATFN "bar_sndNode_kthChild"
// INV: self->ancestor, self->ancestor_gen and self->path are valid
PDCI_node_t *bar_sndNode_kthChild (PDCI_node_t *self, PDCI_childIndex_t idx)
{
  bar *rep=(bar *) (self->rep);
  bar_pd *pd=(bar_pd *) (self->pd);
  bar_m *m=(bar_m *) (self->m);
  PDCI_path_t path = PDCI_PATH_ADD(bar,self->path,idx);
  PDCI_node_t *result = 0;

  // check the validaty of the data
  if (rep == NULL || !PDCI_sndNode_is_valid(self)){
    // in case the right-side of the || got us here:
    self->rep = NULL;  
    self->pd  = NULL;

    switch(idx){
    case 0: 
      // parse descriptor child
      result = PDCI_structured_pd_node_new(self,"pd",NULL,WHATFN);
      PDCI_structured_pd_sndNode_init(result,self->ancestor,self->ancestor_gen,path);
      break;
    case 1:
      result = Pint16_node_new(self,"f1",&(m->f1),NULL,NULL,"element",WHATFN);
      Pint16_sndNode_init(result,self->ancestor,self->ancestor_gen,path);
      break;
    case 2:
      result = Pint32_node_new(self,"f2",&(m->f2),NULL,NULL,"element",WHATFN);
      Pint32_sndNode_init(result,self->ancestor,self->ancestor_gen,path);
      break;
    case 3:
      result = Pchar_node_new(self,"f3",&(m->f3),NULL,NULL,"element",WHATFN);
      Pchar_sndNode_init(result,self->ancestor,self->ancestor_gen,path);
      break;
    }    
  }else {
    switch(idx){
    case 0: 
      // parse descriptor child
      result = PDCI_structured_pd_node_new(self,"pd",pd,WHATFN);
      PDCI_structured_pd_sndNode_init(result,self->ancestor,self->ancestor_gen,path);
      break;
    case 1:
      result = Pint16_node_new(self,"f1",&(m->f1),&(pd->f1),&(rep->f1),"element",WHATFN);
      Pint16_sndNode_init(result,self->ancestor,self->ancestor_gen,path);
      break;
    case 2:
      result = Pint32_node_new(self,"f2",&(m->f2),&(pd->f2),&(rep->f2),"element",WHATFN);
      Pint32_sndNode_init(result,self->ancestor,self->ancestor_gen,path);
      break;
    case 3:
      result = Pchar_node_new(self,"f3",&(m->f3),&(pd->f3),&(rep->f3),"element",WHATFN);
      Pchar_sndNode_init(result,self->ancestor,self->ancestor_gen,path);
      break;
    }
  }  
  return result;
}

Perror_t bar_node_pathWalk(P_t *pads, bar_m *m, bar_pd *pd, bar *rep, PDCI_path_t path,
		      void **m_out, void **pd_out, void **rep_out)
{
  PDCI_childIndex_t idx;
  Perror_t res = P_ERR;
  
  if (path.length > 0){
    // modifies path
    PDCI_PATH_REMOVE(bar,path,idx,path);

    switch(idx){
    case 0: 
      *pd_out = NULL;
      *m_out = NULL;
      res = PDCI_structured_pd_node_pathWalk(pads,(PDCI_structured_pd *)pd,path,rep_out);
      break;
    case 1:
      res = Pint16_node_pathWalk(pads,&(m->f1),&(pd->f1),&(rep->f1),path,m_out,pd_out,rep_out);      
      break;
    case 2:
      res = Pint32_node_pathWalk(pads,&(m->f2),&(pd->f2),&(rep->f2),path,m_out,pd_out,rep_out);      
      break;
    case 3:
      res = Pchar_node_pathWalk(pads,&(m->f3),&(pd->f3),&(rep->f3),path,m_out,pd_out,rep_out);      
     break;
    }
  }else{
    *rep_out = rep;
    *pd_out = pd;
    *m_out = m;

    res = P_OK;
  }

 return res;
}

PDCI_vtable_t const bar_node_vtable={bar_cachedNode_init,
				     bar_node_kthChild,
				     bar_node_kthChildNamed,
				     PDCI_node_free,
				     PDCI_error_typed_value,
				     0};

PDCI_vtable_t const bar_cachedNode_vtable={PDCI_error_cachedNode_init,
					   bar_cachedNode_kthChild,
					   bar_node_kthChildNamed,
					   PDCI_cachedNode_free,
					   PDCI_error_typed_value,
					   0};

PDCI_vtable_t const bar_sndNode_vtable={PDCI_error_cachedNode_init,
					bar_sndNode_kthChild,
					bar_node_kthChildNamed,
					PDCI_node_free,
					PDCI_error_typed_value,
					0};

/**************************************************************************/
/* Bar Array */
/**************************************************************************/


PDCI_node_t *barArray_node_new(PDCI_node_t *parent,
			       const char *name, 
			       void* m, void* pd, void* rep,
			       const char *kind,
			       const char *whatfn){
  PDCI_node_t *result;
  PDCI_MK_NODE (result,&barArray_node_vtable,
		parent,name,m,pd,rep,kind,"barArray_node_new");
  return result;
}
 
PDCI_node_t *barArray_cachedNode_init(PDCI_node_t *self){
  barArray *rep=(barArray *) (self->rep);

  // Setup the virtual table
  self->vt  = & barArray_cachedNode_vtable;

  // Setup node-type specific fields
  self->child_cache = (PDCI_node_t **)PDCI_NEW_LIST(2 + rep->length);
  if(self->child_cache == NULL)
    failwith ("PADS/Galax ALLOC_ERROR: in barArray_cachedNode_init");  

  return self;
}

PDCI_node_t *barArray_node_kthChild (PDCI_node_t *self, PDCI_childIndex_t idx)
{
  barArray *rep=(barArray *) (self->rep);
  barArray_pd *pd=(barArray_pd *) (self->pd);
  barArray_m *m=(barArray_m *) (self->m);
  PDCI_node_t *result = 0;

  switch(idx){
  case 0: // parse descriptor child 
    result = PDCI_sequenced_pd_node_new(self,"pd",pd,"barArray_node_kthChild");
    break;
  case 1: // length field
    result = Puint32_val_node_new(self,"length",&(rep->length),"barArray_node_kthChild");
    break;
  default: // now do elements
    idx -= 2;
    if (idx < rep->length){
      result = bar_node_new(self,"elt",&(m->element),&(pd->elts)[idx],&(rep->elts)[idx],"element","barArray_node_kthChild");
    }
    break;
  }
  return result;
}

PDCI_node_t *barArray_node_kthChildNamed (PDCI_node_t *self, PDCI_childIndex_t idx, const char *name)
{
  barArray *rep=(barArray *) (self->rep);
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

  return (self->vt->kth_child)(self,k);
}

PDCI_node_t *barArray_cachedNode_kthChild (PDCI_node_t *self, PDCI_childIndex_t idx)
{
  barArray *rep=(barArray *) (self->rep);
  PDCI_node_t *result = 0;
  
  if (idx >= 2 + rep->length) // non-existent child
    return result;

  result = self->child_cache[idx];
  if (result == NULL){
    // create a new node for the kth child
    result = barArray_node_kthChild(self,idx);

    // initialize the node to be a cachedNode.
    (result->vt->cachedNode_init)(result);

    // cache the result
    self->child_cache[idx] = result;
  }

  return PDCI_ALIAS_NODE(result);
}

#undef WHATFN
#define WHATFN "barArray_smartNode_kthChild"
PDCI_node_t *barArray_smartNode_kthChild (PDCI_node_t *self, PDCI_childIndex_t idx)
{
  barArray_pd *pd=(barArray_pd *) (self->pd);
  barArray_m *m=(barArray_m *) (self->m);
  
  //void *child_pd  = NULL;
  //void *child_rep = NULL;

  PDCI_node_t *result = 0;
  PDCI_smart_node_t *sn = self->snExt;  
  PDCI_smart_array_info_t *info = (PDCI_smart_array_info_t *)sn->elt_state;
  
  switch(idx){
  case 0: // parse descriptor child 
    result = PDCI_sequenced_pd_node_new(self,"pd",pd,WHATFN);
    break;
  case 1: // length field. Set to number of elements seen so far.
    result = Puint32_val_node_new(self,"length",&info->next_idx_read,WHATFN);
    break;
  default: // now do elements
    idx -= 2;

    if (idx >= info->max_idx)
      break;

    // check whether element[idx] has been created
    // if not, create it lazily.
    if (idx >= info->next_idx_create){
      /* Grow the buffer to the needed size. */
      if (0!=RBuf_reserve (info->_internal,(void **) (&(info->tmap)),sizeof(PDCI_smart_elt_info_t),idx+1,0)) 
	{
	  PDCI_report_err (self->pads,P_LEV_FATAL,0,P_ALLOC_ERR,WHATFN,0);
	}

      info->next_idx_create = idx + 1;

      /* Initialize the element. */
      PDCI_INIT_SMART_ELT(info->tmap[idx],self,idx,pd->loc.b.offset,0,
			  NULL,NULL,&m->element);
      
    }

    result = bar_node_new(self,"elt",&(m->element),info->tmap[idx].pd,info->tmap[idx].rep,"element",WHATFN);
    bar_sndNode_init(result,&info->tmap[idx],info->tmap[idx].gen, PDCI_EMPTY_PATH);

    break;
  }

  return result;
}

PDCI_vtable_t const barArray_node_vtable={barArray_cachedNode_init,
					  barArray_node_kthChild,
					  barArray_node_kthChildNamed,
					  PDCI_node_free,
					  PDCI_error_typed_value,
					  0};

PDCI_vtable_t const barArray_cachedNode_vtable={PDCI_error_cachedNode_init,
						barArray_cachedNode_kthChild,
						barArray_node_kthChildNamed,
						PDCI_cachedNode_free,
						PDCI_error_typed_value,
						0};

PDCI_vtable_t const barArray_dummySmartNode_vtable={PDCI_error_cachedNode_init,
						    barArray_smartNode_kthChild,
						    barArray_node_kthChildNamed,
						    PDCI_cachedNode_free,
						    PDCI_error_typed_value,
						    0};
PDCI_vtable_t const barArray_seqSmartNode_vtable = {PDCI_error_cachedNode_init,
						    barArray_smartNode_kthChild,
						    barArray_node_kthChildNamed,
						    PDCI_cachedNode_free,
						    PDCI_error_typed_value,
						    0};

///////////////////////////////////////////////////////////////////////
//
//   Smart Node related functions
// 
//////////////////////////////////////////////////////////////////////
#define BA_OK 0
#define BA_ERR 1
#define BA_FAIL 2
#define BA_DONE 3

#undef WHATFN
#define WHATFN "barArray_seqSmartNode_eltRead"
/* Currently, only reads sequentially */
Perror_t barArray_seqSmartNode_eltRead(PDCI_node_t *smartNode, P_t *pads, PDCI_smart_elt_info_t *info){
  PDCI_IODISC_2P_CHECKS ("barArray_smartNode_eltRead",smartNode,info);
  {
    PDCI_smart_node_t *sn  = smartNode->snExt;
    PDCI_smart_array_info_t 
      *sa  = (PDCI_smart_array_info_t *)sn->elt_state;
    barArray          *rep  = (barArray *)smartNode->rep;
    barArray_pd       *pd  = (barArray_pd *)smartNode->pd;
    barArray_m        *m   = (barArray_m *)smartNode->m;
    PDCI_childIndex_t  idx = info->idx;
    Puint32 nerr = pd->nerr;		/* Number of array errors before read */
    int result;
    
    // Only allow reading of next element.
    if (idx != sa->next_idx_read){
      return P_ERR;
    }
    
    if (0!=RBuf_reserve (rep->_internal,(void **) (&(rep->elts)),sizeof(bar),idx+1,0)) 
      {
	PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,WHATFN,0);
      }
    if (0!=RBuf_reserve (pd->_internal,(void **) (&(pd->elts)),sizeof(bar_pd),idx+1,0)) 
      {
	PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,WHATFN,0);
      }
    
    result = barArray_read_one(pads,m,pd,rep,idx);	  
    (pd->numRead)++;

    /*
    if (result != BA_OK && P_spec_level (pads) > 0) 
      return P_ERR;
    */

    if (result == BA_FAIL){
      // Mark this idx as the first invalid idx,
      // to prevent reading past the previous element.
      sa->max_idx = idx; 
    }

    sa->next_idx_read++;
    rep->length++;

    info->offset = pd->elts[idx].loc.b.offset;
    info->rep = &rep->elts[idx];
    info->pd = &pd->elts[idx];

    if (result == BA_DONE){
      // Mark the next idx as the first invalid idx,
      // to prevent reading past this element.
      sa->max_idx = idx+1; 
    }
    
    return (pd->nerr == nerr) ? P_OK : P_ERR;

    /* 
    sa->tmap[idx].rep = &rep->elts[idx];
    sa->tmap[idx].pd  = &pd->elts[idx];
    sa->tmap[idx].m  =  &m->element;
    
    return P_OK;

    // Check whether this is the first read.
    if (sa->next_idx_read == 0){
      barArray_pd       *pd  = (barArray_pd *)smartNode->pd;
      barArray_m        *m   = (barArray_m *) smartNode->m;
      int result = barArray_smartRead_init(pads,pd);
      int result = barArray_read_start(pads,pd);
      if (result == P_ERR)
	max_idx = 0;
      }
    */
      
  }  
}


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
 
Perror_t barArray_dummySmartNode_eltPathWalk(P_t *pads, void *m, void *pd, void *rep, PDCI_path_t path,
					     void **m_out, void **pd_out, void **rep_out)
{
  bar    *b_rep = (bar *)rep;
  bar_pd *b_pd  = (bar_pd *)pd;
  bar_m  *b_m   = (bar_m *)m;

  return bar_node_pathWalk(pads,b_m,b_pd,b_rep,path,m_out,pd_out,rep_out);
}

void barArray_dummySmartNode_handleFailure(P_t *pads, PDCI_smart_node_t *node, 
					  PDCI_smart_elt_info_t *info,
					   const char *descr)
{
  error(ERROR_INFO, descr);  
  error(ERROR_FATAL, "*** failure in dummy smart node implementation ***");  
}

/* Sequential version of smartNode_init */
PDCI_node_t *barArray_seqSmartNode_init(PDCI_node_t *self, PDCI_childIndex_t max_idx)
{
  PDCI_smart_array_info_t   *info;
  //PDCI_smart_elt_info_t     *tmap;
  // PDCI_childIndex_t  idx;

  P_t               *pads = self->pads;
  barArray          *rep = (barArray *)self->rep;
  barArray_pd       *pd  = (barArray_pd *)self->pd;
  barArray_m        *m   = (barArray_m *) self->m;

  // Setup the virtual table
  self->vt  = & barArray_seqSmartNode_vtable;

  // Initialize the rep and pd rbufs
  barArray_read_init(pads, m, pd, rep);
  if (barArray_read_start(pads,pd) != P_OK){
    error(0, "barArray_read_start failed");    
    return (PDCI_node_t *) NULL;
  } 

  info = (PDCI_smart_array_info_t *)calloc(1,sizeof(PDCI_smart_array_info_t));
  info->_internal = RMM_new_rbuf (P_rmm_nozero (pads));
  if (0==(info->_internal)) 
    {
      PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,"barArray_seqSmartNode_init","");
    }
  // Ignore offset related fields for now.
  info->next_idx_read = 0;
  info->next_idx_create = 0;
  info->max_idx = max_idx;

  PDCI_MK_SMART_NODE(self->snExt,pads,
		     barArray_seqSmartNode_eltRead,
		     barArray_dummySmartNode_eltFree,
		     barArray_dummySmartNode_eltPathWalk,
		     barArray_dummySmartNode_handleFailure,
		     info,"barArray_seqSmartNode_init");    
  return self;
}

/* Dummy version of smartNode_init */
PDCI_node_t *barArray_dummySmartNode_init(PDCI_node_t *self)
{
  PDCI_smart_array_info_t   *info;
  //PDCI_smart_elt_info_t     *tmap;
  PDCI_childIndex_t  idx;

  P_t               *pads = self->pads;
  barArray          *rep = (barArray *)self->rep;
  barArray_pd       *pd  = (barArray_pd *)self->pd;
  barArray_m        *m   = (barArray_m *) self->m;

  // Setup the virtual table
  self->vt  = & barArray_dummySmartNode_vtable;

  // Setup node-type specific fields

  // Try to read entire array.
  barArray_read_init(pads, m, pd, rep);
  barArray_read_all(pads, m, pd, rep);
  if (P_PS_isPanic(pd)) { 
    error(0, "barArray read raised panic error");    
    return (PDCI_node_t *) NULL;
  } 

  info = (PDCI_smart_array_info_t *)calloc(1,sizeof(PDCI_smart_array_info_t));
  // Allocate the tmap directly, ignoring the rbuf.
  info->tmap = (PDCI_smart_elt_info_t *)calloc(rep->length,sizeof(PDCI_smart_elt_info_t));        		
  // Ignore all fields but next_idx*.
  info->next_idx_read = rep->length;
  info->next_idx_create = rep->length;
  info->max_idx = rep->length - 1;

  for(idx = 0; idx < rep->length; idx++){
    PDCI_INIT_SMART_ELT(info->tmap[idx],self,idx,pd->elts[idx].loc.b.offset,0,
			&rep->elts[idx],&pd->elts[idx],&m->element);
  }

  PDCI_MK_SMART_NODE(self->snExt,pads,
		     barArray_dummySmartNode_eltRead,
		     barArray_dummySmartNode_eltFree,
		     barArray_dummySmartNode_eltPathWalk,
		     barArray_dummySmartNode_handleFailure,
		     info,"barArray_dummySmartNode_init");    
  return self;
}


/*** TEMP: need to be moved elsewhere */

// node->ancestor must be valid.
int PDCI_sndNode_is_valid(PDCI_node_t *node){
  return (node->rep != NULL && node->ancestor->gen == node->ancestor_gen);
}

// use only if !PDCI_node_is_valid(node)
Perror_t PDCI_sndNode_make_valid(PDCI_node_t *node){
  Perror_t res;
  PDCI_smart_elt_info_t  *ancestor = node->ancestor;
  PDCI_smart_node_t      *sn       = ancestor->parent->snExt; // Alias the smart node.

  if (ancestor->rep == NULL) { 
    /* must read from IO stream first */ 
    res = sn->elt_read(ancestor->parent, node->pads, ancestor);
    if (res == P_ERR) { 
      return res;
    } 
  } 

  /* update pointers and generation */ 
  res = sn->elt_path_walk(node->pads,ancestor->m, ancestor->pd, ancestor->rep, node->path,
			  &(node->m), &(node->pd), &(node->rep));
  if (res == P_ERR) { 
    // Something's wrong:
    sn->handle_failure(node->pads,sn,ancestor,"failed to find element in path walk");
    return res;
  } 
  node->ancestor_gen = ancestor->gen; 

  return P_OK;
}

void PDCI_sndNode_validate(PDCI_node_t *node){
  if (!PDCI_sndNode_is_valid(node) 
      && P_ERR == PDCI_sndNode_make_valid(node)){
    failwith("PADS/Galax failed to page node into memory in PGLX_generic_validate_node"); 
  }
}

Perror_t barArray_read_init (P_t *pads,barArray_m *m,barArray_pd *pd,barArray *rep)
{
  PDCI_IODISC_3P_CHECKS ("barArray_read",m,pd,rep);
  PD_COMMON_INIT_NO_ERR (pd);
  {
    Ploc_t tloc;
    rep->length = 0;
    pd->neerr = 0;
    pd->firstError = 0;
    pd->numRead = 0;
    P_io_getLocB (pads,&tloc,0);
    if (0==(rep->_internal)) 
      {
        rep->_internal = RMM_new_rbuf (P_rmm_nozero (pads));
        if (0==(rep->_internal)) 
          {
            PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,"barArray_read","");
          }
      }
    if (0==(pd->_internal)) 
      {
        pd->_internal = RMM_new_rbuf (P_rmm_zero (pads));
        if (0==(pd->_internal)) 
          {
            PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,"barArray_read","");
          }
      }
  }
  return P_OK;
}

/* Returns P_ERR if reading is to be aborted. */
Perror_t barArray_read_start(P_t *pads,barArray_pd *pd)
{
  PDCI_IODISC_1P_CHECKS ("barArray_read_start",pd);

  if ((!P_PS_isPanic (pd))&&(!P_io_at_eof (pads))) 
    {
      // WHY?
      P_io_getLocB (pads,&(pd->loc),0);
      return P_OK;
    }
  return P_ERR;
}

#define BA_OK 0
#define BA_ERR 1
#define BA_FAIL 2
#define BA_DONE 3
/*  
      (*eltCount)-=2;
*/

int barArray_read_one(P_t *pads,barArray_m *m,barArray_pd *pd,barArray *rep, PDCI_childIndex_t idx)
{
  int result;
  Ploc_t beforeLoc,afterLoc;

  PDCI_IODISC_3P_CHECKS ("barArray_read",m,pd,rep);

  // Record current position.
  P_io_getLocB (pads,&beforeLoc,0);

  // read the element
  result = bar_read (pads,&(m->element),&(pd->elts)[idx],&(rep->elts)[idx]);

  // process result
  if (result==P_ERR) 
    {
      // in markErrorSs
      if (P_Test_NotIgnore (m->arrayLevel)) 
	{
	  (pd->neerr)++;
	  if (!(pd->nerr)) 
	    {
	      (pd->nerr)++;
	      pd->errCode = P_ARRAY_ELEM_ERR;
	      P_io_getLocE (pads,&(pd->loc),-1);
	      // Index of first element with an error
	      pd->firstError = idx;
	    }
	}
    }

  if (P_POS_EQ (beforeLoc.b,afterLoc.b)) 
    {
      // array termination from lack of progress
      return BA_FAIL;
    }

  if (P_PS_isPanic (&(pd->elts)[idx])) 
    {
      // No recovery possible
      P_PS_setPanic (pd);
      return BA_DONE;
    }

  // Have we finished reading array?
  if (P_io_at_eof (pads)||P_io_at_eor (pads)) 
    {
      return BA_DONE;
    }

  return (result == P_ERR) ? BA_ERR : BA_OK;
}

Perror_t barArray_read_all(P_t *pads,barArray_m *m,barArray_pd *pd,barArray *rep)
{
  PDCI_IODISC_3P_CHECKS ("barArray_read_all",m,pd,rep);
  {
    int result;

    // Read input until we reach a termination condition
    if (barArray_read_start(pads,pd) == P_OK){
      while (1)
	{
	  (rep->length)++;
	  if (0!=RBuf_reserve (rep->_internal,(void **) (&(rep->elts)),sizeof(bar),rep->length,0)) 
	    {
	      PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,"barArray_read_all",0);
	    }
	  if (0!=RBuf_reserve (pd->_internal,(void **) (&(pd->elts)),sizeof(bar_pd),rep->length,0)) 
	    {
	      PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,"barArray_read_all",0);
	    }

	  result = barArray_read_one(pads,m,pd,rep,rep->length-1);
	  (pd->numRead)++;

	  if (result != BA_OK && P_spec_level (pads) > 0) 
	    return P_ERR;
	  if (result == BA_FAIL){
	    // only -- not -=2 because this read attempt failed, not the previous one.
	    (rep->length)--;
	    break;
	  }
	  if (result == BA_DONE)
	    break;
	}
    }
    return barArray_read_finish(pads,pd,rep);
  }
}

/* return value reflects result of entire read, not just this call. */
Perror_t barArray_read_finish(P_t *pads,barArray_pd *pd,barArray *rep)
{
  PDCI_IODISC_2P_CHECKS ("barArray_read_finish",pd,rep);

  pd->length = (rep->length);
  return ((pd->nerr)==0) ? P_OK : P_ERR;
}
