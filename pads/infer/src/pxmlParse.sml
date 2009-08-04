structure PxmlParse =
  struct
    exception Unexpected

    structure PP = PxmlParseFn(PxmlLex)
    fun parse instrm = 
      let
	val sm = AntlrStreamPos.mkSourcemap()
	val lex = PxmlLex.lex sm
	val strm = PxmlLex.streamifyInstream instrm
	val (r, strm', errs) = PP.parse lex strm
      in
	 r
      end

  fun loadXML filename = 
   let
    val instrm = TextIO.openIn filename
    val result = valOf (parse instrm)
   in result
   end

(***********
   fun main (cmd, args) = 
    if length args < 1 then (print "usage: pxml FILE\n"; raise Unexpected)
    else 
      let val filename = hd args
	  val instrm = TextIO.openIn filename
	  val result = (parse instrm)
	  val _ = case result of
			SOME xml => 
			  let val ty = Pxml.xmlToIR Pxml.StringMap.empty xml
			  in
			    (print "The resulting IR is:\n";
			    Types.printTy ty
			    )
			  end
		      | NONE => print "None\n"
      in 
	  0
      end

   fun emit () =
	(silenceGC ();
	  SMLofNJ.exportFn ("../lib/pxml", main ))


********)
  end
     
