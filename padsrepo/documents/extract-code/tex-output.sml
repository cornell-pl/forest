(* tex-output.sml
 *
 * COPYRIGHT (c) 2002 John Reppy (jhr@cs.uchicago.edu)
 *)

structure TeXDevice : OUTPUT_DEVICE =
  struct

    structure R = OutputReps
    structure T = Tokens

    val suffix = "tex"

    fun revmap _ ([], l) = l
      | revmap f (x::r, l) = revmap f (r, (f x)::l)

  (* this adjusts a block of text to the left by the minimum indent amount.
   * This also reverses the list of lines.
   * NOTE: this ignores RAW text (which is just used for TeX comments).
   *)
    fun outdent lns = let
	  fun extent (R.LN[], minI) = minI
	    | extent (R.LN l, minI) = let
		val {indent, ...} = T.extent l
		in
		  Int.min(minI, indent)
		end
	    | extent (txt, minI) = minI
	  val minIndent = List.foldl extent 10000 lns
	  fun adjust (R.LN({space, kind, text}::r)) =
		R.LN({space=space-minIndent, kind=kind, text=text}::r)
	    | adjust txt = txt
	  in
	    revmap adjust (lns, [])
	  end

    fun output {outStrm, codeEnv, text} = let
	  fun pr s = TextIO.StreamIO.output(outStrm, s)
	  fun pr1 c = TextIO.StreamIO.output1(outStrm, c)
	  fun pr' s = let
		fun prc #"%" = pr "\\%"
		  | prc #"_" = pr "\\_"
		  | prc #"{" = pr "\\{"
		  | prc #"}" = pr "\\}"
		  | prc #"\\" = pr "\\\\"
		  | prc #"$" = pr "\\$"
		  | prc c = pr1 c
		in
		  CharVector.app prc s
		end
	  fun prSpace 0 = ()
	    | prSpace i = (pr1 #" "; prSpace(i-1))
	  fun prText ((T.Keyword | T.Symbol), text) = (pr "\\kw{"; pr' text; pr "}")
	    | prText (T.TyVar, text) = (pr "\\tyvar{"; pr' text; pr "}")
	    | prText (_, text) = pr' text
	  fun putToken {space, kind, text} = (prSpace space; prText (kind, text))
	  fun putHdr (R.COMMENT s :: r) = (pr "% "; pr s; pr "\n"; putHdr r)
	    | putHdr l = (
		pr (concat["\\begin{", codeEnv, "}\n"]);
		putText (false, l);
		pr (concat["\\end{", codeEnv, "}\n"]))
	  and putText (hlight, []) = ()
	    | putText (hlight, elem::r) = putText (
		case elem
		 of (R.RAW s) => (pr s; hlight)
		  | (R.COMMENT s) => (pr "% "; pr s; pr "\n"; hlight)
		  | (R.LN[]) => (pr "\\mbox{}\n"; hlight)
		  | (R.LN toks) => (
		      if hlight
			then (case toks
			   of ({space, kind, text}::r) => (
				prSpace space;
				pr "\\underline{";
				prText (kind, text);
				app putToken r;
				pr "}\n")
			    | _ => pr "\n"
			  (* end case *))
			else (List.app putToken toks; pr "\n");
		      hlight)
		  | R.HLIGHT_ON => true
		  | R.HLIGHT_OFF => false
		(* end case *),
		r)
	  in
	    putHdr (outdent text)
	  end

  end

structure TeXOutput = OutputFn (TeXDevice)
