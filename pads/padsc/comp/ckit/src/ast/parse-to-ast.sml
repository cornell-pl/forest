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

  (* Use this function to convert "sugared" PADS descriptions to
    canonical ones. In particular, expand recursive type declarations
    here. 
  *)
  fun desugarize p = p

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
	val p' = desugarize p
	val result = BuildAst.makeAst (sizes,stateInfo,errState) p
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
