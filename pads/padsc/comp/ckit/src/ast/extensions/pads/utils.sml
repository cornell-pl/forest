structure PTyUtils = 
struct
  open Ast

  fun getCTyName tidtab (cty:Ast.ctype) = 
      let fun getName tid = 
	  let val SOME (tyBinding : Bindings.tidBinding) = Tidtab.find(tidtab,tid)
	  in
	      #name tyBinding
	  end
         fun getIntName(intKind, signedness, signednessTag) =
	     case (intKind, signedness) 
	     of (Ast.INT, Ast.SIGNED) => SOME("xsd:int")
             |  (Ast.FLOAT, _) => SOME("xsd:float")
             | _ => SOME ("ToBeImplemented")
      in
       (case cty 
        of Qual(qual, ctype) => getCTyName tidtab ctype
        |  Numeric(_,_,signedness,intKind,signednessTag) => getIntName (intKind, signedness, signednessTag)
	|  Pointer ctype => getCTyName tidtab ctype
        |  StructRef tid => getName tid
        |  UnionRef tid => getName tid
        |  EnumRef tid => getName tid
        |  TypeRef tid => getName tid
        |  _ => NONE ) 
       handle Option => NONE
      end


  fun bind tidtab tid = 
      let val binding : Bindings.tidBinding = valOf (Tidtab.find(tidtab,tid)) 
      in 
       binding
      end

  fun structInfo tidtab tid =
      let val binding = bind tidtab tid
	  val Bindings.Typedef(tid',cty) = valOf(#ntype binding)
	  val (Ast.StructRef stid) = cty
	  val Bindings.Struct(tid'',fields) = valOf(#ntype (bind tidtab stid))
	  fun cnvField (cty,memOpt : Ast.member option,_,_) = 
	      let val fsym : Symbol.symbol = #name(valOf memOpt)
	      in
		  (getCTyName tidtab cty, SOME (Symbol.name fsym))
	      end
      in
	 (#name binding,List.map cnvField fields)
	 handle Match => (PError.bug "expected typedef to struct binding"; (SOME "bogus", []))
  	 handle Option => (PError.bug "expected SOME"; (SOME "bogus", []))
      end

  fun unionInfo tidtab tid = 
      let val binding = bind tidtab tid
	  val Bindings.Typedef(tid',cty) = valOf(#ntype binding)
	  val (Ast.StructRef stid) = cty
          val Bindings.Struct(tid'',fields) = valOf(#ntype (bind tidtab stid))   (* first field = tag  *)
          val value = List.hd(List.tl(fields))                                   (* second field = val *)
          fun valtid (uctype,_,_,_) = uctype
          val (Ast.TypeRef utid) = valtid value
          val Bindings.Typedef(tid''',utype) = valOf(#ntype (bind tidtab utid))
          val (Ast.UnionRef uutid) = utype
          val Bindings.Union(tid'''',ufields) = valOf(#ntype (bind tidtab uutid)) (* union fields *)
          fun cnvUField (cty,mem : Ast.member,_) =
              let val fsym : Symbol.symbol = #name mem
              in
                  (getCTyName tidtab cty, SOME (Symbol.name fsym))
              end
      in 
	(#name binding,List.map cnvUField ufields)
	handle Match => (PError.bug "expected typedef to struct binding"; (SOME "bogus", []))
	handle Option => (PError.bug "expected SOME"; (SOME "bogus", []))
      end

  fun unionPdInfo tidtab tid =
      let val binding = bind tidtab tid
          val Bindings.Typedef(tid',cty) = valOf(#ntype binding)
          val (Ast.StructRef stid) = cty
          val Bindings.Struct(tid'',pdfields) = valOf(#ntype (bind tidtab stid))    
          val pdfield = (List.last pdfields)
          fun valtid (uctype,_,_,_) = uctype
          val (Ast.TypeRef pdtid) = valtid pdfield
          val Bindings.Typedef(tid''',updtype) = valOf(#ntype (bind tidtab pdtid))
          val (Ast.UnionRef updtid) = updtype
          val Bindings.Union(tid'''',updfields) = valOf(#ntype (bind tidtab updtid))   
          fun cnvField (cty,mem: Ast.member,_) =                            
              let val fsym : Symbol.symbol = #name mem
              in
                  (getCTyName tidtab cty, SOME (Symbol.name fsym))
              end
	  fun cnvPdField (cty,memOpt : Ast.member option,_,_) = 
	      let val fsym : Symbol.symbol = #name(valOf memOpt)
	      in
		  (getCTyName tidtab cty, SOME (Symbol.name fsym))
	      end
      in
	 (#name binding,List.map cnvPdField pdfields,List.map cnvField updfields)
	 handle Match => (PError.bug "expected typedef to struct binding"; (SOME "bogus", [], []))
  	 handle Option => (PError.bug "expected SOME"; (SOME "bogus", [], []))
      end

  fun enumInfo tidtab tid =
      let val binding = bind tidtab tid
	  val Bindings.Typedef(tid',cty) = valOf(#ntype binding)
	  val (Ast.EnumRef stid) = cty
	  val Bindings.Enum(tid'',fields) = valOf(#ntype (bind tidtab stid))
	  fun cnvField (mem: Ast.member,_,_) = 
	      let val fsym : Symbol.symbol = #name mem
	      in
		  (SOME (Symbol.name fsym))
	      end
      in
	 (#name binding,List.map cnvField fields)
	 handle Match => (PError.bug "expected typedef to struct binding"; (SOME "bogus", []))
  	 handle Option => (PError.bug "expected SOME"; (SOME "bogus", []))
      end

  fun typedefInfo tidtab tid =
      let val binding = bind tidtab tid
	  val Bindings.Typedef(tid',cty) = valOf(#ntype binding)
	  val (Ast.TypeRef stid) = cty
          val bindTy = bind tidtab stid
      in
	 (#name binding,#name bindTy)
	 handle Match => (PError.bug "expected typedef to struct binding"; (SOME "bogus", SOME "bogus type"))
  	 handle Option => (PError.bug "expected SOME"; (SOME "bogus", SOME "bogus type"))
      end
end