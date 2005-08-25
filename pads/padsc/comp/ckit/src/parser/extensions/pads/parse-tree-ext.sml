structure ParseTreeExt = 
  struct
        datatype Pty = 
           Name of string

        datatype 'exp PSize = 
           SizeInfo of {min : 'exp option, max : 'exp option, maxTight : bool}

        datatype 'exp PTermExp = Expr of 'exp | noSep

        datatype 'exp PPostCond = 
           General of 'exp
         | ParseCheck of 'exp

        datatype 'exp PConstraint = 
           Sep   of 'exp
         | Term  of 'exp PTermExp
         | Last  of 'exp PPostCond list 
         | Ended of 'exp PPostCond list 
         | Skip  of 'exp PPostCond list 
         | Longest

        datatype 'exp  OptPredicate = Simple of 'exp PPostCond list
	                        | Decon of {some : (string * 'exp PPostCond list) option, none : 'exp PPostCond list option}
       
        datatype ('ct, 'dt, 'decr, 'exp) PSField = 
           Full of {    pty : Pty, 
		       args : 'exp list, 
		       name : string, 
		  isVirtual : bool,
		   isEndian : bool,
                   isRecord : bool,
             containsRecord : bool,
             largeHeuristic : bool,
		       pred : 'exp PPostCond list option, 
		    comment : string option,
		    optDecl : bool,
                    optPred : ('exp OptPredicate) option,
		  arrayDecl : bool, 
		       size : ('exp PSize) option,
		  arraypred : ('exp PConstraint) list}
	 | Manifest of {tyname : 'ct,
			name   : string,
			 args  : 'exp list,
	             isVirtual : bool,
			 expr  : 'exp,
			 pred  : 'exp PPostCond list option,
                      comment  : string option}
         | Brief of 'exp * string option


        datatype 'exp PRange = 
           ArrayName of string
         | Bounds of 'exp * 'exp


        datatype 'exp PArrayPostCond = 
           Forall of {index : string, range : 'exp PRange, body : 'exp}
         | AGeneral of 'exp
         | AParseCheck of 'exp


        datatype ('ct,'dt, 'decr, 'exp) PBranches = 
           Ordered of (('ct, 'dt, 'decr, 'exp) PSField) list
         | Switched of {descriminator : 'exp,
			cases         : 'exp option list,
			branches      : (('ct, 'dt, 'decr, 'exp) PSField) list}

        type 'exp  PPredicate =  {predTy: Pty, thisVar : string, pred : 'exp}
        datatype ('decr, 'ct, 'dt, 'exp) PExternal = 
           PTypedef of {name : string,
			params: ('ct * 'decr) list, 
			isRecord : bool, 
                        containsRecord : bool, 
                        largeHeuristic : bool,
			isSource : bool,
                        baseTy: Pty,
			args   : 'exp list, 
			pred : ('exp PPredicate) option}
         | PRecursive of {name : string,
			params: ('ct * 'decr) list, 
			isRecord : bool, 
                        containsRecord : bool, 
			isSource : bool,
                        base: {name:string,args:'exp list} option}
         | PDynamic of {name : string,
			params: ('ct * 'decr) list, 
			isRecord : bool, 
                        containsRecord : bool, 
			isSource : bool,
                        baseTy: Pty,
			args   : 'exp list}
         | Popt of     {name : string,
			params: ('ct * 'decr) list, 
			args   : 'exp list,
			isRecord : bool, 
			isSource : bool,
                        pred : ('exp OptPredicate) option,
                        baseTy: Pty}
         | PStruct of {isAlt : bool,
		       name : string, 
		       params: ('ct * 'decr) list, 
		       isRecord : bool, 
                       containsRecord : bool, 
                       largeHeuristic : bool,
		       isSource : bool,
		       fields : (('ct, 'dt, 'decr, 'exp) PSField) list,
		       postCond : ('exp PPostCond) list}
         | PArray  of {name : string, 
		       baseTy : Pty, 
		       params : ('ct * 'decr) list, 
		       isRecord : bool, 
                       containsRecord : bool, 
                       largeHeuristic : bool,
		       isSource : bool,
		       args   : 'exp list, 
		       sizeSpec : ('exp PSize) option, 
		       constraints : ('exp PConstraint) list,
		       postCond : ('exp PArrayPostCond) list} 
         | PUnion of {name     : string,
		      params   : ('ct * 'decr) list,
		      isLongestMatch : bool, 
		      isRecord : bool, 
		      isSource   : bool,
                      containsRecord : bool, 
                      largeHeuristic : bool,
		      variants : ('ct,'dt, 'decr, 'exp) PBranches,
		      postCond : ('exp PPostCond) list,
		      fromOpt : bool}
         | PEnum of  {name     : string,
                      params   : ('ct * 'decr) list,
		      isRecord : bool, 
                      containsRecord : bool, 
                      largeHeuristic : bool,
		      isSource   : bool,
		      prefix   :  string option,
                      members  : (string * string option * 'exp option * string option) list}
         | PSelect of {selName : string,
		       tyName  : string,
		       varName : string,
		       path    : 'exp}
         | PCharClass of {name : string,
		          pred : 'exp}
         | PDone

        datatype PStatement = PComment of string
        datatype ('exp) PExpression = Pregexp of 'exp

						 
	datatype PDeclaration = 
		 (* A phantom decl takes a type name and a var name. *)
		 PPhantomDecl of string * string

  (* External bindings *)
	type operatorExt = unit

	type ('spec,'decr,'ct,'dt,'oper,'exp,'stmt) 
	    expressionExt = ('exp) PExpression

	type ('spec,'decr,'ct,'dt,'oper,'exp,'stmt) 
	    specifierExt = unit

	type ('spec,'decr,'ct,'dt,'oper,'exp,'stmt) 
	    declaratorExt = unit
	    
	type ('spec,'decr,'ct,'dt,'oper,'exp,'stmt) 
	    statementExt = PStatement

	type ('spec,'decr,'ct,'dt,'oper,'exp,'stmt) 
	    declarationExt = PDeclaration

	type ('spec,'decr,'ct,'dt,'oper,'exp,'stmt) 
	    externalDeclExt = ('decr, 'ct, 'dt, 'exp) PExternal 
    end


