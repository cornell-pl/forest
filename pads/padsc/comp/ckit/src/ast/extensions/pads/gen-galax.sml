(** generation of common PADS-Galax stuff **)

structure GenGalax = struct

  structure PT   = ParseTree     (* the parse tree *)
  structure P    = ParseTreeUtil (* Utility functions for manipulating the parse tree *)
  structure PX   = ParseTreeExt  (* Pads extensions *)
  structure PL   = PLib          (* Information about values/functions available from pads library *)
  structure PN   = PNames

  val kind = "kind"
  val whatfn = "whatfn"
  val result    = "result"

  val self      = "self"
  val parent = "parent"
  val idx       = "idx"  
  val childName = "name"
  val nameStr = childName
  val elt =  "elt"
  val manager = "manager"
  val ancIdx = "ancestor_idx"
  val gen = "gen"
  val path = "path"
  val pads = "pads"

  (* auxiliary functions *)
  fun apply [] x = []
    | apply (f::fs) x = (f x)::(apply fs x)

  fun inc x = x + 1

  fun listOf n = List.tabulate (n, inc)

  fun enumerate xs = ListPair.zip(listOf (List.length xs), xs)

  fun getUniqueTys nil = nil
    | getUniqueTys ((_,fieldTy,_)::fs) = 
      let val utys = getUniqueTys fs
	  fun notIn (fd,nil) = true
	    | notIn (fd,f::fs) = fd <> f andalso notIn (fd,fs)	
      in 
	  if notIn(fieldTy,utys) then fieldTy::utys
	  else utys
      end

  (* header: common declaration part in foo_children function *) 
  fun headerGalaxChildrenFun(nameTy) =
      let val nodeRepTy = PL.nodeT
	  fun varDecl(field, ty) = 
	      let fun typePref n = P.ptrPCT (P.makeTypedefPCT n)		
		  val typeField = typePref ty
	      in P.varDeclS(typeField, field, PT.Cast(typeField, P.fieldX(self, field)))
	      end
      in List.map varDecl (ListPair.zip([PN.rep, PN.pd, PN.m], (apply [PN.repSuf, PN.pdSuf, PN.mSuf] nameTy)))
	 @ [P.varDeclS'(P.ptrPCT (P.ptrPCT nodeRepTy), result)]
      end

  (* header: common declaration part in foo_kth_child function *) 
  fun headerGalaxKthChildFun(nameTy) =
      let val nodeRepTy = PL.nodeT
	  fun varDecl(field, ty) = 
	      let fun typePref n = P.ptrPCT (P.makeTypedefPCT n)		
		  val typeField = typePref ty
	      in P.varDeclS(typeField, field, PT.Cast(typeField, P.fieldX(self, field)))
	      end
      in List.map varDecl (ListPair.zip([PN.rep, PN.pd, PN.m], (apply [PN.repSuf, PN.pdSuf, PN.mSuf] nameTy)))
	 @ [P.varDeclS(P.ptrPCT nodeRepTy, result, P.zero)]
      end

  (* if: common if-then in foo_children function *)
  fun ifGalaxChildren(returnName, number, errorString) =
      [PT.IfThen(P.notX(P.assignX(returnName,
				  PT.Call(PL.PDCI_NEW_NODE_PTR_LIST, 
					  [number]))),		
		 PT.Expr(PT.Call(PT.Id "failwith", [PT.String ("PADS/Galax " ^ errorString)])))]

  (* PDCI_MK_TNODE: common in foo_children function
     Takes a *general* result expression.*)
  fun macroTNodeCallGeneral (returnExpr, structId, valStr, valId, cnvName) = 
      [PT.Expr(PT.Call(PL.PDCI_MK_TNODE,	
		       [returnExpr,
			P.addrX(PT.Id(PN.nodeVTableSuf structId)),
			PT.Id self, 
			PT.String valStr, 
			valId,
			PT.String cnvName]))]

  (* PDCI_MK_TNODE: common in foo_children function *)
  fun macroTNodeCall (returnName, index, structId, valStr, valId, cnvName) = 
      [PT.Expr(PT.Call(PL.PDCI_MK_TNODE,	
		       [P.subX(returnName, index), 
			P.addrX(PT.Id(PN.nodeVTableSuf structId)),
			PT.Id self, 
			PT.String valStr, 
			valId,
			PT.String cnvName]))]

  fun macroTNode (returnName, structId, valStr, valId, cnvName) = 
      (P.mkCommentS "parse descriptor child")
      :: macroTNodeCall(returnName, P.zero, structId, valStr, valId, cnvName)

  fun macroTNodeGeneral (returnName, structId, valStr, valId, cnvName) = 
      (P.mkCommentS "parse descriptor child")
      :: macroTNodeCallGeneral(returnName, structId, valStr, valId, cnvName)

  (* calls PDCI_MK_NODE. Takes a *general* result expression. *)
  fun macroNodeCallGeneral (returnExp, tyField, nameField, getField1, getField2, getField3, nameStruct) = 
      PT.Expr(PT.Call(PL.PDCI_MK_NODE,
		      [returnExp, 
		       P.addrX(PT.Id(PN.nodeVTableSuf tyField)),
		       PT.Id self, 
		       nameField, 
		       getField1, getField2, getField3,
		       PT.String "element",
		       PT.String nameStruct])) 

  (* calls PDCI_MK_NODE. Assumes that result expression is an array element.
	   Therefore, takes array name "returnName" and index "n". common in foo_children function *)
  fun macroNodeCall (returnName, n, tyField, nameField, getField1, getField2, getField3, nameStruct) = 
      macroNodeCallGeneral(P.subX(returnName, n),tyField,nameField,
			   getField1,getField2,getField3,nameStruct)

  (* calls NODE_NEW_BODY macro*)
  fun macroNodeNew(ty) =
		    PT.Expr(PT.Call(PL.NODE_NEW_BODY,
				    [PT.Id ty]))

  (* calls NODE_NEW_RET macro*)
  fun macroNodeNewRet() = PT.Call(PL.NODE_NEW_RET,nil)

  (* calls CACHED_NODE_INIT_BODY macro*)
  fun macroCNInit(ty,numChildrenExpr) =
		    PT.Expr(PT.Call(PL.CACHED_NODE_INIT_BODY,
				    [PT.Id ty, numChildrenExpr]))

  (* calls CACHED_NODE_INIT_RET macro*)
  fun macroCNInitRet() = PT.Call(PL.CACHED_NODE_INIT_RET,nil)

  (* calls CACHED_NODE_INIT_BODY macro*)
  fun macroCNKthChild(ty,numChildrenExpr) =
		    PT.Expr(PT.Call(PL.CACHED_NODE_KTH_CHILD_BODY,
				    [PT.Id ty, numChildrenExpr]))

  (* calls CACHED_NODE_INIT_RET macro*)
  fun macroCNKthChildRet() = PT.Call(PL.CACHED_NODE_KTH_CHILD_RET,nil)

  (* calls ARR_NODE_KTH_CHILD_BODY macro*)
  fun macroArrKC(ty,childTy) = 
      PT.Expr(PT.Call(PL.ARR_NODE_KTH_CHILD_BODY,
		      [PT.Id ty,PT.Id childTy]))

  (* calls ARR_NODE_KTH_CHILD_BODY macro*)
  fun macroArrKCRet() = PT.Call(PL.ARR_NODE_KTH_CHILD_RET,nil)

  fun macroArrLength(ty) = PT.Call(PL.ARR_LENGTH, [PT.Id ty])

  (* calls ARR_NODE_KTH_CHILD_BODY macro*)
  fun macroArrKCN(ty) =
	  PT.Expr(PT.Call(PL.ARR_NODE_KTH_CHILD_NAMED_BODY,
			  [PT.Id ty]))

  (* calls ARR_NODE_KTH_CHILD_RET macro*)
  fun macroArrKCNRet() =
		    PT.Call(PL.ARR_NODE_KTH_CHILD_NAMED_RET,nil)

  (* calls STR_NODE_KTH_CHILD_BODY_BEGIN macro*)
  fun macroStructKCBegin(ty) =
      PT.Expr(PT.Call(PL.STR_NODE_KTH_CHILD_BODY_BEGIN,
		      [PT.Id ty]))

  (* calls STR_NODE_KTH_CHILD_BODY_END macro*)
  fun macroStructKCEnd() =
		    PT.Expr(PT.Call(PL.STR_NODE_KTH_CHILD_BODY_END,
				    nil))

  (* calls STR_NODE_KTH_CHILD_RET macro*)
  fun macroStructKCRet() =
		    PT.Call(PL.STR_NODE_KTH_CHILD_RET,nil)

  (* calls NODE_KC_CASE macro*)
  fun macroKCCase(ty,n,fieldTy,fieldName) =
      PT.Expr(PT.Call(PL.NODE_KC_CASE,
		      [PT.Id ty,P.intX n,PT.Id fieldTy,PT.Id fieldName]))

  (* calls NODE_KC_CASE_COMP macro*)
  fun macroKCCaseComp(ty,n,fieldTy,fieldName) =
      PT.Expr(PT.Call(PL.NODE_KC_CASE_COMP,
	      [PT.Id ty,P.intX n,PT.Id fieldTy,PT.Id fieldName]))

  fun makeKCCase name (n, (fieldName, fieldTy, isPcomputed)) =
      if isPcomputed then
	  macroKCCaseComp(name,n,fieldTy,fieldName)				      
      else
	  macroKCCase(name,n,fieldTy,fieldName)				      

  (* calls STR_NODE_KTH_CHILD_BODY macro*)
  fun macroStructKCN(ty,fieldNames) =
      let val fnList = map PT.String fieldNames
      in
	  PT.Expr(PT.Call(PL.STR_NODE_KTH_CHILD_NAMED_BODY,
			  (PT.Id ty) :: fnList))
      end

  (* calls STR_NODE_KTH_CHILD_RET macro*)
  fun macroStructKCNRet() =
		    PT.Call(PL.STR_NODE_KTH_CHILD_NAMED_RET,nil)

  fun macroSNDInit(ty) = 
      PT.Expr(PT.Call(PL.SND_NODE_INIT_BODY,[PT.Id ty]))
  fun macroSNDInitRet() = 
      PT.Call(PL.SND_NODE_INIT_RET,nil)

  fun macroSNDKCCase(ty,n,fieldTy,fieldName) =
      PT.Expr(PT.Call(PL.SND_NODE_KC_CASE,
		      [PT.Id ty,P.intX n,PT.Id fieldTy,PT.Id fieldName]))
  fun macroSNDKCCaseComp(ty,n,fieldTy,fieldName) =
      PT.Expr(PT.Call(PL.SND_NODE_KC_CASE_COMP,
		      [PT.Id ty,P.intX n,PT.Id fieldTy,PT.Id fieldName]))

  fun macroStructSNDKCBegin(ty) =
      PT.Expr(PT.Call(PL.STR_SND_NODE_KTH_CHILD_BODY_BEGIN,[PT.Id ty]))
  fun macroStructSNDKCEnd() =
      PT.Expr(PT.Call(PL.STR_SND_NODE_KTH_CHILD_BODY_END,nil))
  fun macroStructSNDKCRet() = 
      PT.Call(PL.STR_SND_NODE_KTH_CHILD_RET,nil)

  fun makeSNDKCCase name (n, (fieldName, fieldTy, isPcomputed)) =
      if isPcomputed then
	  macroSNDKCCaseComp(name,n,fieldTy,fieldName)
      else
	  macroSNDKCCase(name,n,fieldTy,fieldName)				      

  (* calls STR_NODE_PATH_WALK_BODY_BEGIN macro*)
  fun macroStructPWBegin() =
      PT.Expr(PT.Call(PL.STR_NODE_PATH_WALK_BODY_BEGIN,nil))

  (* calls STR_NODE_PATH_WALK_BODY_END macro*)
  fun macroStructPWEnd() =
		    PT.Expr(PT.Call(PL.STR_NODE_PATH_WALK_BODY_END,
				    nil))

  (* calls STR_NODE_PATH_WALK_RET macro*)
  fun macroStructPWRet() =
		    PT.Call(PL.STR_NODE_PATH_WALK_RET,nil)

  (* calls NODE_PW_CASE macro*)
  fun macroPWCase(n,fieldTy,fieldName) =
      PT.Expr(PT.Call(PL.NODE_PW_CASE,
		      [P.intX n,PT.Id fieldTy,PT.Id fieldName]))

  (* calls NODE_PW_CASE_COMP macro*)
  fun macroPWCaseComp(n,fieldTy,fieldName) =
      PT.Expr(PT.Call(PL.NODE_PW_CASE_COMP,
	      [P.intX n,PT.Id fieldTy,PT.Id fieldName]))

  fun makePWCase (n, (fieldName, fieldTy, isPcomputed)) =
      if isPcomputed then
	  macroPWCaseComp(n,fieldTy,fieldName)				      
      else
	  macroPWCase(n,fieldTy,fieldName)				      

  fun macroArrSNDKCBody(ty,childTy) =
      PT.Expr(PT.Call(PL.ARR_SND_NODE_KTH_CHILD_BODY,[PT.Id ty, PT.Id childTy]))

  fun macroArrSNDKCRet() = 
      PT.Call(PL.ARR_SND_NODE_KTH_CHILD_RET,nil)

  fun macroArrPWBody(childTy) =
      PT.Expr(PT.Call(PL.ARR_NODE_PATH_WALK_BODY,[PT.Id childTy]))

  fun macroArrPWRet() = 
      PT.Call(PL.ARR_NODE_PATH_WALK_RET,nil)

  (* 
   galax non-existent file name: Bogus file name for use
   with MARK decls, to prevent the pretty printer from
   printing the contained decl 
   *)
  val NEFName = ".remove_pads"

  fun makeInvisibleDecls(tys,fields) =
      let fun makeDecl ty name = 
	      PT.Decl (PT.DeclarationExt (PX.PPhantomDecl (ty, name)))

	  val tyDecls = map (makeDecl "type_t") tys
	  val fieldDecls = map (makeDecl "field_t") fields 
      in 
	  tyDecls @ fieldDecls
      end

  fun makeNodeNewFun(name) =		
      let val nodeRepTy = PL.nodeT
	  val returnTy = P.ptrPCT nodeRepTy
	  val cnvName = PN.nodeNewSuf name
	  val paramTys = [returnTy, P.ccharPtr, 
			  P.voidPtr,P.voidPtr,P.voidPtr,
			  P.ccharPtr,P.ccharPtr]
	  val paramNames = [parent,nameStr,
			    PN.m,PN.pd,PN.rep,
			    kind,whatfn]
	  val formalParams =  List.map P.mkParam (ListPair.zip(paramTys, paramNames))

	  val bodySs = makeInvisibleDecls([name],nil)
		       @ [macroNodeNew(name)]
		       @ [P.returnS (macroNodeNewRet())]
      in   
	  P.mkFunctionEDecl(cnvName, formalParams, PT.Compound bodySs, returnTy)
      end
			      
  fun makeCNInitFun(name,numChildrenExpr) =		
      let val nodeRepTy = PL.nodeT
	  val returnTy = P.ptrPCT nodeRepTy
	  val cnvName = PN.cnInitSuf name
	  val paramTys = [returnTy]
	  val paramNames = [self]
	  val formalParams =  List.map P.mkParam (ListPair.zip(paramTys, paramNames))

	  val bodySs = makeInvisibleDecls([name],nil)
		       @ [macroCNInit(name,numChildrenExpr)]
		       @ [P.returnS (macroCNInitRet())]
      in   
	  P.mkFunctionEDecl(cnvName, formalParams, PT.Compound bodySs, returnTy)
      end

  fun makeCNKCFun(name,numChildrenExpr) = 
      let val nodeRepTy = PL.nodeT
	  val returnTy = P.ptrPCT nodeRepTy
	  val cnvName = PN.cnKCSuf name
	  val paramTys = [P.ptrPCT nodeRepTy, PL.childIndexT]
	  val paramNames = [self,idx]
	  val formalParams =  List.map P.mkParam (ListPair.zip(paramTys, paramNames))

	  val bodySs = makeInvisibleDecls([name],nil)
		       @ [macroCNKthChild(name,numChildrenExpr)]     
		       @ [P.returnS (macroCNKthChildRet())]
      in   
	  P.mkFunctionEDecl(cnvName, formalParams,PT.Compound bodySs, returnTy)
      end
			      

  fun makeSNDInitFun(name) =		
      let val nodeRepTy = PL.nodeT
	  val managerTy = P.ptrPCT PL.managerT   
	  val genTy = PL.genT                     
	  val childTy = PL.childIndexT                   
	  val returnTy = P.ptrPCT nodeRepTy
	  val cnvName = PN.sndInitSuf name
	  val paramTys = [returnTy, managerTy, childTy, genTy, childTy]
	  val paramNames = [self,manager,ancIdx,gen,idx]
	  val formalParams =  List.map P.mkParam (ListPair.zip(paramTys, paramNames))

	  val bodySs = makeInvisibleDecls([name],nil)
		       @ [macroSNDInit(name)]     
		       @ [P.returnS (macroSNDInitRet())]
      in   
	  P.mkFunctionEDecl(cnvName, formalParams, PT.Compound bodySs, returnTy)
      end
			      
  fun makeStructSNDKthChildFun(name,fields) =		
      let val nodeRepTy = PL.nodeT
	  val returnTy = P.ptrPCT nodeRepTy
	  val cnvName = PN.sndKCSuf name
	  val paramTys = [P.ptrPCT nodeRepTy, PL.childIndexT]
	  val paramNames = [self,idx]
	  val formalParams =  List.map P.mkParam (ListPair.zip(paramTys, paramNames))

	  val uniqueFieldTys = getUniqueTys fields
	  val fieldNames = map (fn (n,_,_) => n) fields						   
	  val bodySs = makeInvisibleDecls(name :: uniqueFieldTys, fieldNames)
		       @ [macroStructSNDKCBegin(name)] 
		       @ (List.map (makeSNDKCCase name) (enumerate fields)) 
		       @ [macroStructSNDKCEnd(),   
			  P.returnS (macroStructSNDKCRet())] 
      in   
	  P.mkFunctionEDecl(cnvName, formalParams, PT.Compound bodySs, returnTy)
      end

(* Perror_t ty ## _node_pathWalk(P_t *pads, ty ## _m *m, ty ## _pd *pd, ty  *rep, PDCI_path_t path,
		      void **m_out, void **pd_out, void **rep_out) *)
  fun makeStructPathWalkFun(name,fields) =		
      let val padsTy = P.ptrPCT PL.toolStatePCT
	  val returnTy = PL.toolErrPCT
	  val cnvName = PN.nodePWSuf name
	  val maskTy = P.ptrPCT (P.makeTypedefPCT (PN.mSuf name))
	  val pdTy  = P.ptrPCT (P.makeTypedefPCT (PN.pdSuf name))
	  val repTy  = P.ptrPCT (P.makeTypedefPCT (PN.repSuf name))
	  val pathTy = PL.pathT
	  val vppT = P.voidPtrPtr
	  val paramTys = [padsTy,maskTy,pdTy,repTy,pathTy,vppT,vppT,vppT]
	  val paramNames = [pads,PN.m,PN.pd,PN.rep,path,"m_out","pd_out","rep_out"]
	  val formalParams =  List.map P.mkParam (ListPair.zip(paramTys, paramNames))

	  val uniqueFieldTys = getUniqueTys fields
	  val fieldNames = map (fn (n,_,_) => n) fields						   
	  val bodySs = makeInvisibleDecls(name :: uniqueFieldTys, fieldNames)
		       @ [macroStructPWBegin()] 
		       @ (List.map makePWCase (enumerate fields)) 
		       @ [macroStructPWEnd(),   
			  P.returnS (macroStructPWRet())] 
      in   
	  P.mkFunctionEDecl(cnvName, formalParams, PT.Compound bodySs, returnTy)
      end

  fun makeArrSNDKthChildFun(name,childName) =		
      let val nodeRepTy = PL.nodeT
	  val returnTy = P.ptrPCT nodeRepTy
	  val cnvName = PN.sndKCSuf name
	  val paramTys = [P.ptrPCT nodeRepTy, PL.childIndexT]
	  val paramNames = [self,idx]
	  val formalParams =  List.map P.mkParam (ListPair.zip(paramTys, paramNames))

	  val bodySs = makeInvisibleDecls([name,childName], nil)
		       @ [macroArrSNDKCBody(name,childName)] 
		       @ [P.returnS (macroArrSNDKCRet())] 
      in   
	  P.mkFunctionEDecl(cnvName, formalParams, PT.Compound bodySs, returnTy)
      end

  fun makeArrPathWalkFun(name,childName) =		
      let val padsTy = P.ptrPCT PL.toolStatePCT
	  val returnTy = PL.toolErrPCT
	  val cnvName = PN.nodePWSuf name
	  val maskTy = P.ptrPCT (P.makeTypedefPCT (PN.mSuf name))
	  val pdTy  = P.ptrPCT (P.makeTypedefPCT (PN.pdSuf name))
	  val repTy  = P.ptrPCT (P.makeTypedefPCT (PN.repSuf name))
	  val pathTy = PL.pathT
	  val vppT = P.voidPtrPtr
	  val paramTys = [padsTy,maskTy,pdTy,repTy,pathTy,vppT,vppT,vppT]
	  val paramNames = [pads,PN.m,PN.pd,PN.rep,path,"m_out","pd_out","rep_out"]
	  val formalParams =  List.map P.mkParam (ListPair.zip(paramTys, paramNames))

	  val bodySs = makeInvisibleDecls([childName], nil)
		       @ [macroArrPWBody(childName)] 
		       @ [P.returnS (macroArrPWRet())] 
      in   
	  P.mkFunctionEDecl(cnvName, formalParams, PT.Compound bodySs, returnTy)
      end

  fun makeNodeVtable(name) =
	  PT.ExternalDecl(PT.Declaration({specifiers=[PL.PDCI_vtable_t], qualifiers=[PT.CONST], storage=[]},
					 [(PT.VarDecr (PN.nodeVTableSuf name),
					   PT.InitList [PT.Id(PN.cnInitSuf name),
							PT.Id(PN.nodeKCSuf name),
							PT.Id(PN.nodeKCNSuf name),
							PL.PDCI_node_free,
							PL.PDCI_error_typed_value,
							P.zero])])) 

(*
const PDCI_vtable_t \
ty ## _cachedNode_vtable = {PDCI_error_cachedNode_init, \
			     ty ## _cachedNode_kthChild, \
			     ty ## _node_kthChildNamed, \
                             PDCI_cachedNode_free, \
			     PDCI_error_typed_value, \
			     PDCI_not_impl_yet_string_value};\

*)
  fun makeCachedNodeVtable(name) =
	  PT.ExternalDecl(PT.Declaration({specifiers=[PL.PDCI_vtable_t], qualifiers=[PT.CONST], storage=[]},
					 [(PT.VarDecr (PN.cnVTableSuf name),
					   PT.InitList [PL.PDCI_error_cachedNode_init,
							PT.Id(PN.cnKCSuf name),
							PT.Id(PN.nodeKCNSuf name),
							PL.PDCI_cachedNode_free,
							PL.PDCI_error_typed_value,
							P.zero])])) 

  fun makeSNDNodeVtable(name) =
	  PT.ExternalDecl(PT.Declaration({specifiers=[PL.PDCI_vtable_t], qualifiers=[PT.CONST], storage=[]},
					 [(PT.VarDecr (PN.sndVTableSuf name),
					   PT.InitList [PL.PDCI_error_cachedNode_init,
							PT.Id(PN.sndKCSuf name),
							PT.Id(PN.nodeKCNSuf name),
							PL.PDCI_node_free,
							PL.PDCI_error_typed_value,
							P.zero])])) 

end
