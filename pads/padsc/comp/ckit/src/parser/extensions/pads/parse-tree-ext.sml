structure ParseTreeExt = 
  struct
       datatype Pty = 
           Name of string

       datatype ('ct, 'exp) PSField = 
           Full of {pty : Pty, name : string, pred : 'exp option}
         | Brief of 'exp
       
       datatype ('ct, 'exp) PExternal = 
          PStruct of {name : string, fields : (('ct, 'exp) PSField) list}

  (* External bindings *)
	type operatorExt = unit

	type ('spec,'decr,'ct,'dt,'oper,'exp,'stmt) 
	    expressionExt = unit

	type ('spec,'decr,'ct,'dt,'oper,'exp,'stmt) 
	    specifierExt = unit

	type ('spec,'decr,'ct,'dt,'oper,'exp,'stmt) 
	    declaratorExt = unit
	    
	type ('spec,'decr,'ct,'dt,'oper,'exp,'stmt) 
	    statementExt = unit

	type ('spec,'decr,'ct,'dt,'oper,'exp,'stmt) 
	    declarationExt = unit

	type ('spec,'decr,'ct,'dt,'oper,'exp,'stmt) 
	    externalDeclExt = ('ct, 'exp) PExternal 
    end


