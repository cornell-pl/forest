structure Main = 
  struct
    fun silenceGC () = (SMLofNJ.Internals.GC.messages false)

    fun main (_, [fname]) = let
      val strm = TextIO.openIn fname
      val lex = WordLex.makeLexer (fn n => TextIO.inputN (strm, n))
      fun count (accum) = (case lex()
	of SOME () => count (accum + 1)
	 | NONE => accum
       (* end case *))
      in
        print (Int.toString (count 0) ^ " words in " ^ fname ^ ".\n")
	before TextIO.closeIn strm;
        OS.Process.exit(OS.Process.success)
      end

    fun emit () = 
	    (silenceGC();
	     SMLofNJ.exportFn ("lib/scan", main ))

  end