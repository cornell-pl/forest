structure ParseTreeExt = 
  struct
        datatype Pty = 
           Name of string

        datatype ('dt, 'decr, 'exp) PSField = 
           Full of {    pty : Pty, 
		       args : 'exp list, 
		       name : string, 
		  isVirtual : bool,
		   isEndian : bool,
		       pred : 'exp option, 
		    comment : string option}
	 | Manifest of { decl : 'dt * ('decr * 'exp) list, comment : string option}
         | Brief of 'exp
         | EOR

        datatype 'exp PSize = 
           SizeInfo of {min : 'exp option, max : 'exp option, maxTight : bool}

        datatype 'exp PRange = 
           ArrayName of string
         | Bounds of 'exp * 'exp

        datatype 'exp PConstraint = 
           Sep of 'exp
         | Term of 'exp
         | Forall of {index : string, range : 'exp PRange, body : 'exp}
         | General of 'exp

        datatype ('dt, 'decr, 'exp) PBranches = 
           Ordered of (('dt, 'decr, 'exp) PSField) list
         | Switched of {descriminator : 'exp,
			cases         : 'exp option list,
			branches      : (('dt, 'decr, 'exp) PSField) list}
       
        datatype ('decr, 'ct, 'dt, 'exp) PExternal = 
           PTypedef of {name : string,
			params: ('ct * 'decr) list, 
			isRecord : bool,
                        baseTy: Pty,
			args   : 'exp list, 
                        predTy: Pty,
			thisVar: string,
			pred: 'exp}
         | PStruct of {name : string, 
		       params: ('ct * 'decr) list, 
		       isRecord : bool,
		       fields : (('dt, 'decr, 'exp) PSField) list}
         | PArray  of {name : string, 
		       baseTy : Pty, 
		       params : ('ct * 'decr) list, 
		       isRecord : bool,
		       args   : 'exp list, 
		       sizeSpec : ('exp PSize) option, 
		       constraints : ('exp PConstraint) list} 
         | PUnion of {name     : string,
		      params   : ('ct * 'decr) list,
		      isRecord : bool,
		      variants : ('dt, 'decr, 'exp) PBranches}
         | PEnum of  {name     : string,
                      params   : ('ct * 'decr) list,
		      isRecord : bool,
                      members  : (string * 'exp option * string option) list}

        datatype PStatement = PComment of string

  (* External bindings *)
	type operatorExt = unit

	type ('spec,'decr,'ct,'dt,'oper,'exp,'stmt) 
	    expressionExt = unit

	type ('spec,'decr,'ct,'dt,'oper,'exp,'stmt) 
	    specifierExt = unit

	type ('spec,'decr,'ct,'dt,'oper,'exp,'stmt) 
	    declaratorExt = unit
	    
	type ('spec,'decr,'ct,'dt,'oper,'exp,'stmt) 
	    statementExt = PStatement

	type ('spec,'decr,'ct,'dt,'oper,'exp,'stmt) 
	    declarationExt = unit

	type ('spec,'decr,'ct,'dt,'oper,'exp,'stmt) 
	    externalDeclExt = ('decr, 'ct, 'dt, 'exp) PExternal 
    end


