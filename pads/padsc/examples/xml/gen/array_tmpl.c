void Init(){
  if (max_length == 0) {
    set needFinalChecks;
  }

  length = 0;
  set state = PARTIAL;
  META( Pterm ) => set needTerm;
}

typedef int ReadOneResult;

ReadOneResult 
read_one(P_t *pads,ty_m *m,ty_pd *pd,ty *rep,
	 elemTy_pd *elt_pd, elemTy *elt_rep,
         void *extra_params)
{
  /** Used by skip case to read the next element. */
  int readAgain = 0;
 
  if (not set PARTIAL)
    return ALREADY_DONE;
  if (set panic) {
    META( Psep | Pterm ) =>
      search_for_sep_or_term;
    META( Precord ) => 
      search_for_EOR;  
    META( _ ) => 
      return PANIC_SKIP;
  }

  /**
   * INV: Not Panicking.
   */
  status = ONGOING;

  if (needFinalChecks || EOF())
    goto lbl_finalChecks;
  
  do{
    readAgain = 0;

    set beginLoc;
    META( Pended ) => 
      checkpoint();
    if (pd->numRead > 0){
      META( Psep && Pterm ) => {
	search_for(sep | term);
	if (not_found)
	  panic;
	else{
	  if (not_found_immediate)
	    report_error;
	  if (found term){
	    unset needTerm;
	    commit_if_necessary;
	    goto lbl_finalChecks;
	  }	    
	}
      }
      META( Psep ) => see_above;
      META( Pterm ) => see_above;
    }
    
    /**
     * INV: Ready to read element.
     */
 
    res = elemTy_read(...);
    rep->length++;
    pd->numRead++;
    
    set endLoc;
    if (no_progress){
      P_PS_setPanic(pd);
      commit_if_necessary;
      status = NO_PROGRESS;
      goto lbl_finalChecks;
    }

    if (P_PS_isPanic (elt_pd)) {
      P_PS_setPanic(pd);
    }

    META( Pskip(test1) ) =>
      if (test1()){
	commit_if_necessary;
	rep->length--;
	readAgain == 1;
      }
     
  }while(readAgain);

  if (P_ERR == res){
    pd->neerr++;
    etc_etc;
  }

  META( Pended(test1,outparam1) ) => {
    (isEnded,putback) = test1();

    if (isEnded) {
      if (putback){
	rep->length--;
	restore_io;
      }else{
	commit;
      }

      goto lbl_finalChecks;
    }

    commit;
  }

  /**
   * INV: Checkpoint committed or restored.
   */

  META( Plast(test1) ) =>
    if (test1()) 
      goto lbl_finalChecks;
  META( MAX ) => {
    if (rep->length == MAX) 
      goto lbl_finalChecks;
    if (rep->length > MAX)
      internal_error("should not reach this point");
  }
  META( Pterm ) =>
    if (is_term peek(currentPos)){
      unset needTerm;
      goto lbl_finalChecks;
    }

  /**
   * INV: Not termination condition satisfied.
   */

  return ONGOING;

 lbl_finalChecks:
  unset needFinalChecks;
  unset PARTIAL;

  META( Pterm ) => {
    if (needTerm){
      unset needTerm;
      scan_for_term;
      if (found_term && found_extra_stuff)
        pd->nerr++;
      else if (!found_term)
        set_panic;      
    }
  }

  META( MIN ) => {
    if (m->arrayLevel says_user &&
	rep->length < MIN)
      nerr+;
  }

  META( Pwhere(cond) ) => {
    if (m->arrayLevel says_user &&
	cond())
      etc_etc;
  }

  META( Precord ) => {
    similar_to_term_code;
  }

  return DONE;
}
