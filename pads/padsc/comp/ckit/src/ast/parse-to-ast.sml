(* parse-to-ast.sml *)

structure ParseToAst : PARSE_TO_AST =
struct

  type astBundle =
    {ast: Ast.ast,
     tidtab: Bindings.tidBinding Tidtab.uidtab,
     errorCount: int,
     warningCount: int,
     auxiliaryInfo: {aidtab: Tables.aidtab,
		     paidtab : Tables.paidtab,
	             implicits: Tables.aidtab,
                     env: State.symtab}}
	
  structure PT = ParseTree
  structure PX = ParseTreeExt
  datatype recTy = OPAQUE | TRANSPARENT of string

  (* Use this function to convert "sugared" PADS descriptions to
    canonical ones. In particular, expand recursive type declarations
    here. 
  *)
  fun desugarize ps = 
      let 
	  fun remove (ctxt,key) = 
	      let fun rmv (nil,_,ctxt') = (ctxt',NONE)
		    | rmv ((elt as (k,_))::ctxt,key:string,ctxt') =
		      if  k = key then (ctxt' @ ctxt,SOME elt) 
		      else rmv(ctxt,key,elt::ctxt')
	      in rmv (ctxt,key,nil) end

	  fun dsugPads (ctxt,ede) = 
	      let fun checkAndReplace name isSource isRecord 
				      containsRecord reconstruct =
		      let val (ctxt,elt) = remove(ctxt, name)
			  val eds = 
			      case elt of
				  NONE => [ede]
				| SOME (_, rparams) =>
				  let val recname = PNames.recPre name
				  in
				      [reconstruct recname,
				       PX.PDynamic {name=name,params=rparams,
						    baseTy=PX.Name recname,
						    isSource=isSource,isRecord=isRecord,
						    containsRecord=containsRecord}]
				  end
(* 				| SOME (_,TRANSPARENT dname) => *)
(* 				  [ede, *)
(* 				   PX.PDynamic {name=dname,params=[], *)
(* 						args=[],baseTy=PX.Name name, *)
(* 						isSource=isSource,isRecord=isRecord, *)
(* 						containsRecord=containsRecord}] *)
		      in
			  (ctxt,eds)
		      end
	      in   
		  case ede of 
		      (* Support for this feature has been removed. *)
(* 		      PX.PRecursive {base=SOME{name=bname,...},name,...} *)
(* 		      => ((bname,TRANSPARENT(name))::ctxt,[ede]) *)
		      PX.PRecursive {base=NONE,name,params,...} 
		      => ((name, params)::ctxt,[ede])
		    | PX.PStruct {name, isAlt,params,isRecord,containsRecord,
				  largeHeuristic,isSource,fields,postCond} => 
		      let fun reconstruct newname =
			      PX.PStruct 
				  {name=newname, isAlt=isAlt,params=params,
				   isRecord=isRecord, containsRecord=containsRecord,
				   largeHeuristic=largeHeuristic, isSource=isSource,
				   fields=fields, postCond=postCond}
		      in checkAndReplace name isSource isRecord containsRecord reconstruct end
		    | PX.PUnion {name, params,isLongestMatch,isRecord,isSource,
				 containsRecord,largeHeuristic,variants,postCond,
				 fromOpt} =>
		      let fun reconstruct newname =
			      PX.PUnion
				  {name=newname, params=params, isLongestMatch=isLongestMatch, 
				   isRecord=isRecord,isSource=isSource, containsRecord=containsRecord,
				   largeHeuristic=largeHeuristic,variants=variants,
				   postCond=postCond,fromOpt=fromOpt}
		      in checkAndReplace name isSource isRecord containsRecord reconstruct end
		    | PX.PArray {name,baseTy,params,isRecord,containsRecord,largeHeuristic, 
				 isSource,args,sizeSpec,constraints,postCond} =>
		      let fun reconstruct newname =
			      PX.PArray
				  {name=newname, baseTy=baseTy, params=params, 
				   isRecord=isRecord, containsRecord=containsRecord, 
				   largeHeuristic=largeHeuristic, isSource=isSource, 
				   args=args, sizeSpec=sizeSpec, constraints=constraints, 
				   postCond=postCond}
		      in checkAndReplace name isSource isRecord containsRecord reconstruct end
(* 		    | PX.PTypedef {name, params, isRecord, containsRecord, largeHeuristic,  *)
(* 				isSource, baseTy, args, pred} => *)
(* 		      let fun reconstruct newname = *)
(* 			      PX.PTypedef  *)
(* 				  {name=newname, params=params, isRecord=isRecord,  *)
(* 				   containsRecord=containsRecord,  *)
(* 				   largeHeuristic=largeHeuristic, isSource=isSource,  *)
(* 				   baseTy=baseTy, args=args, pred=pred}		       *)
(* 		      in checkAndReplace name isSource isRecord containsRecord reconstruct end *)
		    | PX.Popt {name, params, args, isRecord, isSource, pred, baseTy} =>
		      let fun reconstruct newname =
			      PX.Popt 
				  {name=newname, params=params, args=args, isRecord=isRecord,
				   isSource=isSource, pred=pred, baseTy=baseTy}
		      in checkAndReplace name isSource isRecord false reconstruct end
		    | _ => (ctxt,[ede])
	      end

	  fun dsug (ctxt, PT.ExternalDeclExt ede) = 
	      let val (c,edes) = dsugPads (ctxt,ede) in
		  (c,map PT.ExternalDeclExt edes)
	      end
	    | dsug (ctxt, PT.MARKexternalDecl (loc,ed)) = 
	      let val (ctxt',eds) = dsug (ctxt,ed)
		  fun mark ed = PT.MARKexternalDecl (loc,ed) 
	      in (ctxt',map mark eds) end
	    | dsug (ctxt,ed) = (ctxt,[ed])

	  fun desugar (ed, (ctxt, allEds)) = 
	      let val (ctxt', eds) = dsug (ctxt,ed)
	      in (ctxt',eds::allEds) end

	  val (ctxt,allEds) = foldl desugar (nil,nil)  ps
      (* Should we check here that the ctxt is empty? *)
      in
	  foldl (op @) nil allEds
      end
	  

  fun progToState ({tidtab, auxiliaryInfo={aidtab, paidtab,implicits, env}, ...} : astBundle) =
      State.STATE({ttab=tidtab,atab=aidtab,ptab = paidtab, implicits=implicits},env)

  fun fileToAst' errStrm (sizes: Sizes.sizes, stateInfo: State.stateInfo) inFile
         : astBundle = 
      let
	(* suppress underscores to make error message more readable *)
	val suppressPidUnderscores = !PPLib.suppressPidUnderscores
	val suppressTidUnderscores = !PPLib.suppressTidUnderscores
	val _ = (PPLib.suppressPidUnderscores := true;
		 PPLib.suppressTidUnderscores := true)
	val errState = Error.mkErrState errStrm
	val p = Parser.parseFile errState inFile
	val canPT = desugarize p (* get a canonical parse tree *)
	val result = BuildAst.makeAst (sizes,stateInfo,errState) canPT
      in
	PPLib.suppressPidUnderscores := suppressPidUnderscores;
	PPLib.suppressTidUnderscores := suppressTidUnderscores;
	result
      end
    
  fun fileToAst inFile =
    fileToAst' TextIO.stdErr (Sizes.defaultSizes, State.INITIAL) inFile

  fun fileToC x = 
      let val {ast, tidtab, ...} = fileToAst x
       in PPLib.ppToStrm (PPAst.ppAst PPAst.ALL NONE () tidtab) TextIO.stdOut ast
      end

end (* structure ParseToAst *)
