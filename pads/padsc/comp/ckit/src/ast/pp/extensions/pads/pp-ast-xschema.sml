local 
  structure PPAstPaidAdornment : PPASTPAIDADORNMENT =
  struct
    type aidinfo = unit
    type paidinfo = Tables.paidtab

    fun ppExpressionAdornment ppCoreExpr aidinfo tidtab pps (Ast.EXPR (coreExpr,_,_)) = 
	ppCoreExpr aidinfo tidtab pps coreExpr

    fun ppStatementAdornment ppCoreStmt aidinfo tidtab pps  (Ast.STMT (coreStmt,_,_)) = 
	ppCoreStmt aidinfo tidtab pps coreStmt


    fun ppExternalDeclAdornment srcFileOpt paidinfo ppCoreExternalDecl aidinfo tidtab pps
	  (Ast.DECL (coreExtDecl,_,paid:Paid.uid,loc:SourceMap.location)) = 
          (case Paidtab.find(paidinfo, paid) 
           of NONE => print "No pads info.\n"
           | _ => ppCoreExternalDecl aidinfo tidtab pps coreExtDecl)          
  end

in
  structure PPXSchemaAst = PPAstXschemaFn(structure PPAstPaidAdornment=PPAstPaidAdornment)
end
