structure ParseTreeExt = 
  struct
        datatype Pty = 
           Name of string

        datatype ('ct, 'exp) PSField = 
           Full of {pty : Pty, name : string, pred : 'exp option, comment : string option}
         | Brief of 'exp

        datatype 'exp PSize = 
           SizeInfo of {min : 'exp option, max : 'exp option, maxTight : bool}

        datatype 'exp PConstraint = 
           Sep of 'exp
         | Term of 'exp
         | Forall of {index : string, arrayName : string, body : 'exp}
         | General of 'exp
       
        datatype ('ct, 'exp) PExternal = 
           PStruct of {name : string, fields : (('ct, 'exp) PSField) list}
         | PArray  of {name : string, baseTy : Pty, 
		       sizeSpec : ('exp PSize) option, 
		       constraints : ('exp PConstraint) list} 

        datatype PStatement = 
          PComment of string

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
	    externalDeclExt = ('ct, 'exp) PExternal 
    end


