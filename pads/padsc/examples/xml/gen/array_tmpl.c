typedef struct ty_roArgs_s ty_roArgs_t;

  struct ty_roArgs_s{
    Ploc_t tloc;
    int foundTerm;
    int reachedLimit;
    stdecls();
  };

blah read_elem(...)
{
  decls;
  regexp separator_regexp_ptr;
  regexp terminator_regexp_ptr;  
  Ploc_t tloc;
  Ploc_t *loc_ptr = &tloc;
  int res;

  init(...);
  AR_READ_ALL(ty_read_one(...),"read_elem");    
  return AR_STD_RETURN();
}

ReadResult_t Init(P_t *pads,ty_m *m,ty_pd *pd,ty *rep,
		  META( cParams ) => cParams
		  ){

  META( Pterm ) => int foundTerm = 0;
  META( MAX ) => int reachedLimit = 0;
  rep->length = 0;
  pd->neerr = 0;
  pd->firstError = 0;
  pd->numRead = 0;
  P_PS_setPartial(pd);

  /* Check for inconsistencies. */
  META( Psep && Pterm ) => {
    AR_SEP_TERM_DYN_CHK();
  }

  META( MIN && MAX ) =>
    AR_TEST_MIN_GT_MAX(list);

  /* Check for termination conditions. */
  AR_TEST_FC_PANIC(STATUS_BREAK);

  META( Pterm ) => {
    AR_TEST_FC_MATCH_TERM(matchFun,...);
  }

  META( MAX ) =>
    if (max == 0) {
      reachedLimit = 1;
      goto do_final_checks;
    }

  P_io_getLocB (pads,&(pd->loc),0);

  return AR_RET_ONGOING();

  AR_LBL_FINAL_CHECKS();
  return doFinalChecks(pads,m,
		       META( cParams ) => cParams,
		       pd,rep,
		       META( Pterm ) => foundTerm,
		       META( MAX ) => reachedLimit
}

/**
 * INV: Not Panicking.
 */

ReadResult_t
read_one(P_t *pads,ty_m *m,t...,
	 ty_pd *pd,ty *rep,
	 elemTy_pd *elt_pd, elemTy *elt_rep)
{
  META( Pterm ) => 
    int foundTerm = 0;
  AR_RO_DECS();

  AR_TEST_ALREADY_DONE();
  
  AR_GET_BEGIN_LOC();
  
  META( Pended ) => 
    checkpoint();
  
  if (pd->numRead > 0){
    META( Psep && Pterm ) => AR_SCAN_SEP_TERM(...);      
    META( Psep ) => AR_SCAN_SEP(...);
  }
  
  /**
   * INV: Ready to read element.
   */
  
  /* READ_ELEMENT */
  result = elemTy_read(...);
  rep->length++;
  pd->numRead++;
  
  META((Pended || Plast) && Pparsecheck...) =>
    P_io_getLocE (pads,&(pd->loc),0);    

  META( Pomit(test1) ) =>
    if (test1()){
      commit_if_necessary;
      rep->length--;
      pd->numRead--;
    }
  
  
  META( Pomit(test1) ) =>
    //if (!omittedResult){
      
  META( Pended(test1,outparam1) ) => {
    (isEnded,consume) = test1();
    
    if (!isEnded) {
      commit;
    }else{
      if (consume){
	commit;
      }else{
	restore_io;
	rep->length--;
	pd->numRead--;
      }
      
      goto do_final_checks;
    }

  }

  AR_TEST_READ_ERR(1);	

  /* Panic recovery */
  if (P_PS_isPanic (elt_pd)) {
    P_PS_setPanic(pd);
    ...
  }
  
  META( Pomit(test1) ) =>
    //}

  /**
   * INV: Checkpoint committed or restored.
   */

  META( Plast(test1) ) =>
    if (test1()) 
      goto do_final_checks;
  META( Pterm ) =>
    AR_TEST_MATCH_TERM(...)
  META( MAX ) => 
    AR_TEST_REACHED_MAX(max);

  AR_TEST_FC_REACHED_BREAK();

  /* CHECK_PROGRESS */
  set endLoc;
  if (no_progress){
    commit_if_necessary;
    goto do_final_checks;
  }
  
  return AR_RET_ONGOING();

  AR_LBL_FINAL_CHECKS();
  return AR_RET_FINAL_CHECKS();
}

ReadResult_t
ty ## _final_checks(P_t *pads,ty_m *m,ty_pd *pd,ty *rep,
	       int foundTerm,
	       void *extra_params){
  P_PS_unsetPartial(pd);

  META( Pterm ) => AR_TEST_MISSING_TERM(...)

  META( Precord ) => AR_READ_EOR(...);

  META( MIN ) => AR_CHK_ENOUGH_ELEMENTS(...)

  META( Pwhere(cond) ) => 
    // Checking user-defined array constraints
    if (P_Test_SemCheck (m->arrayLevel)) 
      {
	...
      }

  return AR_RET_DONE();
}
