#undef WHATFN
#define WHATFN "barArray_smartNode_kthChild"
PDCI_node_t *barArray_smartNode_kthChild (PDCI_node_t *self, PDCI_childIndex_t idx)
{
  barArray_pd *pd=(barArray_pd *) (self->pd);
  barArray_m *m=(barArray_m *) (self->m);  
  PDCI_node_t *result = 0;
  PDCI_smart_node_t *sn = self->snExt;  
  PDCI_smart_array_info_t *arrayInfo = (PDCI_smart_array_info_t *)sn->elt_state;
  
  switch(idx){
  case 0: /*  parse descriptor child */
    result = PDCI_sequenced_pd_node_new(self,"pd",pd,WHATFN);
    break;
  case 1: /* length field. Set to number of elements seen so far. */
    result = Puint32_val_node_new(self,"length",&arrayInfo->next_idx_read,WHATFN);
    break;
  default: /*  now do elements */
    idx -= 2;

    if (!P_PS_isPartial(pd) && idx >= arrayInfo->next_idx_read)
      break;

    // check whether element[idx] has been created
    // if not, create it and read in the data.
    if (idx >= arrayInfo->next_idx_create){
      /* Grow the buffer to the needed size. */
      if (0!=RBuf_reserve (arrayInfo->_internal,(void **) (&(arrayInfo->tmap)),sizeof(PDCI_smart_elt_info_t),idx+1,0)) 
	{
	  PDCI_report_err (self->pads,P_LEV_FATAL,0,P_ALLOC_ERR,WHATFN,0);
	}

      arrayInfo->next_idx_create = idx + 1;

      /* Initialize the element info. */
      PDCI_INIT_SMART_ELT(arrayInfo->tmap[idx],self,idx,0,0,
			  NULL,NULL,&m->element);
      
      /* Attempt to read the element */
      if (P_ERR == sn->elt_read(self, self->pads, &arrayInfo->tmap[idx])) { 
	// Check whether anything was read.
	if (!P_PS_isPartial(pd)) 
	  return result; // array finished so nothing read; return the default node.
	// else, go on as normal.
      }
    }

    result = bar_node_new(self,"elt",&(m->element),arrayInfo->tmap[idx].pd,arrayInfo->tmap[idx].rep,"element",WHATFN);
    bar_sndNode_init(result,sn,idx,arrayInfo->tmap[idx].gen, idx+2);

    break;
  }

  return result;
}

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

unsigned int barArray_seqSmartNode_chooseIdxToEvict(PDCI_smart_array_info_t *arrayInfo){
  arrayInfo->next_idx_evict %= arrayInfo->max_elts;
  return arrayInfo->next_idx_evict++;
}

void barArray_seqSmartNode_evictIdx(PDCI_smart_array_info_t *arrayInfo, unsigned int i){
  PDCI_childIndex_t j = arrayInfo->invMap[i];
  arrayInfo->tmap[j].rep = NULL;
  arrayInfo->tmap[j].pd = NULL;
  arrayInfo->tmap[j].gen++;
}

void barArray_seqSmartNode_setInvIdx(PDCI_smart_array_info_t *arrayInfo, unsigned int i, PDCI_childIndex_t j){
  arrayInfo->invMap[i]=j;  
}

#undef WHATFN
#define WHATFN "barArray_seqSmartNode_getFreeIdx"
PDCI_childIndex_t barArray_seqSmartNode_getFreeIdx(PDCI_node_t *smartNode, P_t *pads){
  PDCI_smart_node_t *sn  = smartNode->snExt;
  PDCI_smart_array_info_t 
    *arrayInfo  = (PDCI_smart_array_info_t *)sn->elt_state;
  barArray          *rep  = (barArray *)smartNode->rep;
  barArray_pd       *pd  = (barArray_pd *)smartNode->pd;
  PDCI_childIndex_t  physIdx = 0;

  // should we allocate more memory or kick something out?
  if (rep->length < arrayInfo->max_elts){
    if (0!=RBuf_reserve (rep->_internal,(void **) (&(rep->elts)),sizeof(bar),rep->length+1,0)) 
      {
	PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,WHATFN,0);
      }
    if (0!=RBuf_reserve (pd->_internal,(void **) (&(pd->elts)),sizeof(bar_pd),rep->length+1,0)) 
      {
	PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,WHATFN,0);
      }
    if (0!=RBuf_reserve (arrayInfo->_internal_inv,(void **) (&(arrayInfo->invMap)),sizeof(PDCI_childIndex_t),rep->length+1,0)) 
      {
	PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,WHATFN,0);
      }
    physIdx = rep->length++;
  }else{
    physIdx = barArray_seqSmartNode_chooseIdxToEvict(arrayInfo);
    barArray_seqSmartNode_evictIdx(arrayInfo,physIdx);
  }

  return physIdx;
}


/* XXX TEMP XXX */
Sfio_t *P_io_get(P_t *pads)
{
  return pads->io;
}

Perror_t P_io_seek(P_t *pads, Sfoff_t offset){
  Sfio_t *io = P_io_get(pads);
  P_io_close(pads);
  if (-1 == sfseek(io, offset, 0)) {
    return P_ERR;
  }
  return P_io_set(pads, io);
}

#undef WHATFN
#define WHATFN "barArray_seqSmartNode_eltRead"
/* Currently, only reads sequentially */
Perror_t barArray_seqSmartNode_eltRead(PDCI_node_t *smartNode, P_t *pads, PDCI_smart_elt_info_t *info){
  PDCI_IODISC_2P_CHECKS (WHATFN,smartNode,info);
  {
    PDCI_smart_node_t *sn  = smartNode->snExt;
    PDCI_smart_array_info_t 
      *arrayInfo  = (PDCI_smart_array_info_t *)sn->elt_state;
    barArray          *rep  = (barArray *)smartNode->rep;
    barArray_pd       *pd  = (barArray_pd *)smartNode->pd;
    barArray_m        *m   = (barArray_m *)smartNode->m;
    PDCI_childIndex_t  virtIdx = info->idx;
    PDCI_childIndex_t  physIdx = 0;
    Puint32 nerr = pd->nerr;		/* Number of array errors before read */
    int result;
    
    // are we trying to read ahead?
    if (virtIdx > arrayInfo->next_idx_read){
      return P_ERR;
    }

    physIdx = barArray_seqSmartNode_getFreeIdx(smartNode,pads);
    barArray_seqSmartNode_setInvIdx(arrayInfo,physIdx,info->idx);
    
    // are we trying to read an earlier element?
    if (virtIdx < arrayInfo->next_idx_read){
      Sfoff_t cur_offset;
      PDCI_IO_GETOFFSET(pads,cur_offset);

      // should we seek?
      if (cur_offset != info->offset){
	
	// seek to info->offset.
	if (P_io_seek(pads,info->offset) == P_ERR){
	  error(ERROR_FATAL, "*** failure in " WHATFN ": seek failed ***");  
	}
	
      }
      // read the rep,pd
      bar_read (pads,&(m->element),&pd->elts[physIdx],&(rep->elts)[physIdx]);

      // we don't process a result because this element has been
      // read/processed already.

      // reset the element info
      info->rep = &rep->elts[physIdx];
      info->pd = &pd->elts[physIdx];
    
      // did we seek?
      if (cur_offset != info->offset){
	// are we still reading the array?
	if (P_PS_isPartial(pd)){

	  // INV: if the current offset >= nextOffset then the current
	  // offset is the farthest point in the stream that we've
	  // read (the head of the stream).  Otherwise nextOffset is
	  // the head of the stream.  

	  // Therefore, if we're at the head of the stream and we're
	  // seeking backwards, then we must save the offset of the
	  // head in nextOffset.

	  // are we at the head of the stream?
	  if (cur_offset > arrayInfo -> next_offset){
	    arrayInfo->next_offset = cur_offset;
	  }
	}else{
	  // We are no longer reading the array. Therefore, we must
	  // return the io stream to its former position so that other
	  // pads read calls can proceed as normal.

	  // seek to previous offset.
	  if (P_io_seek(pads,cur_offset) == P_ERR){
	    error(ERROR_FATAL, "*** failure in " WHATFN ": seek failed ***");  
	  }
	}
      }

      return P_OK;
    }

    // are we about to read for the first time?
    if (arrayInfo->next_idx_read == 0){
      if (barArray_read_start(pads,pd) != P_OK){
	return P_ERR;
      } 
      arrayInfo->first_offset = pd->loc.b.offset;
    }
    
    result = barArray_read_one(pads,m,pd,rep,physIdx);	  
    (pd->numRead)++;

    /*
    if (result != BA_OK && P_spec_level (pads) > 0) 
      return P_ERR;
    */

    if (result == BA_FAIL){
      // we're done reading the array:
      P_PS_unsetPartial(pd);

      return P_ERR;
    }

    arrayInfo->next_idx_read++;
    
    info->offset = pd->elts[physIdx].loc.b.offset;
    info->rep = &rep->elts[physIdx];
    info->pd = &pd->elts[physIdx];

    if (result == BA_DONE){
      // we're done reading the array:
      P_PS_unsetPartial(pd);
    }
    
    return (pd->nerr == nerr) ? P_OK : P_ERR;
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
PDCI_node_t *barArray_seqSmartNode_init(PDCI_node_t *self,  unsigned int max_elts)
{
  PDCI_smart_array_info_t   *arrayInfo;
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

  // Initialize the array info.
  arrayInfo = (PDCI_smart_array_info_t *)calloc(1,sizeof(PDCI_smart_array_info_t));
  arrayInfo->_internal = RMM_new_rbuf (P_rmm_nozero (pads));
  if (0==(arrayInfo->_internal)) 
    {
      PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,"barArray_seqSmartNode_init","");
    }
  arrayInfo->_internal_inv = RMM_new_rbuf (P_rmm_nozero (pads));
  if (0==(arrayInfo->_internal_inv)) 
    {
      PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,"barArray_seqSmartNode_init","");
    }
  arrayInfo->max_elts = max_elts;

  arrayInfo->first_offset = 0;
  arrayInfo->next_offset = 0;

  arrayInfo->next_idx_read = 0;
  arrayInfo->next_idx_create = 0;
  arrayInfo->next_idx_evict = 0;
  P_PS_setPartial(pd);
  //arrayInfo->max_idx = max_idx;

  PDCI_MK_SMART_NODE(self->snExt,pads,
		     barArray_seqSmartNode_eltRead,
		     barArray_dummySmartNode_eltFree,
		     barArray_dummySmartNode_eltPathWalk,
		     barArray_dummySmartNode_handleFailure,
		     arrayInfo,"barArray_seqSmartNode_init");    
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
  //  info->max_idx = rep->length - 1;

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
PDCI_smart_elt_info_t *PDCI_get_ancestor(PDCI_node_t *node){
  PDCI_smart_array_info_t *arrayInfo = (PDCI_smart_array_info_t *)node->manager->elt_state;
  PDCI_childIndex_t idx = node->ancestor_idx;
  return &arrayInfo->tmap[idx];
}

// PDCI_get_ancestor(node) must be valid.
int PDCI_sndNode_is_valid(PDCI_node_t *node){
  return (node->rep != NULL && PDCI_get_ancestor(node)->gen == node->ptr_gen);
}

// use only if !PDCI_node_is_valid(node)
Perror_t PDCI_sndNode_make_valid(PDCI_node_t *node){
  Perror_t res;
  PDCI_smart_elt_info_t  *ancestor = PDCI_get_ancestor(node);
  PDCI_smart_node_t      *sn       = ancestor->parent->snExt; // Alias the smart node.
  PDCI_path_t             path;

  if (ancestor->rep == NULL) { 
    /* must read from IO stream first */ 
    res = sn->elt_read(ancestor->parent, node->pads, ancestor);
    if (res == P_ERR) { 
      return res;
    } 
  } 

  path = PDCI_node_getPath(node,0);

  /* update pointers and generation */ 
  res = sn->elt_path_walk(node->pads,ancestor->m, ancestor->pd, ancestor->rep, path,
			  &(node->m), &(node->pd), &(node->rep));

  PDCI_PATH_FREE(path);

  if (res == P_ERR) { 
    // Something's wrong:
    sn->handle_failure(node->pads,sn,ancestor,"failed to find element in path walk");
    return res;
  } 
  node->ptr_gen = ancestor->gen; 

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
    // Ploc_t tloc; NEVER USED?
    rep->length = 0;
    pd->neerr = 0;
    pd->firstError = 0;
    pd->numRead = 0;
    // P_io_getLocB (pads,&tloc,0);
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

  // Record current position.
  P_io_getLocB (pads,&afterLoc,0);

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
