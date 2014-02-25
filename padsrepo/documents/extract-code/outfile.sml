(* outfile.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

structure OutFile : sig

    type outfile

    val openFile : (Atom.atom * Tokens.attr list) -> outfile
    val close : outfile -> unit

    val nameOf : outfile -> Atom.atom
    val sameFile : (outfile * outfile) -> bool

    val enable : outfile -> unit
    val disable : outfile -> unit

    val highlight : bool -> outfile -> unit

    val mbox : outfile -> unit

    val line : Tokens.line -> outfile -> unit
    val text : string -> outfile -> unit

  end = struct

    structure SS = Substring
    structure T = Tokens

    datatype block_kind = CODE | CENTER

    datatype text
      = RAW of string
      | MBOX
      | LN of T.line
      | HLIGHT_ON | HLIGHT_OFF

    fun revmap _ ([], l) = l
      | revmap f (x::r, l) = revmap f (r, (f x)::l)

  (* this adjusts a block of text to the left by the minimum indent amount.
   * This also reverses the list of lines.
   * NOTE: this ignores RAW text (which is just used for TeX comments).
   *)
    fun outdent lns = let
	  fun extent (LN[], minI) = minI
	    | extent (LN l, minI) = let
		val {indent, ...} = T.extent l
		in
		  Int.min(minI, indent)
		end
	    | extent (txt, minI) = minI
	  val minIndent = List.foldl extent 10000 lns
	  fun adjust (LN({space, kind, text}::r)) =
		LN({space=space-minIndent, kind=kind, text=text}::r)
	    | adjust txt = txt
	  in
	    revmap adjust (lns, [])
	  end

    datatype outfile = OF of {
	name : Atom.atom,	(* the output file name *)
	codeEnv : string,	(* type of code environment (code, centercode, *)
				(* or tightcode) *)
	closed : bool ref,	(* false, while the file is open *)
	enabled : bool ref,	(* true, if output is enabled *)
	text : text list ref	(* the output to the file *)
      }

(****
fun prFile ({name, enabled, ...} : out_file) =
print(Format.format "%s [%b]" [Format.STR(Atom.toString name), Format.BOOL(!enabled)])
fun prFiles [] = print "{}"
  | prFiles [f] = (print "{"; prFile f; print "}")
  | prFiles (f::r) = (
      print "{"; prFile f; app (fn f => (print ", "; prFile f)) r; print "}")
****)

    fun openFile (name, attrs) = let
	  val codeEnv = (case attrs
		 of (Tokens.LEFT::_) => "code"
		  | (Tokens.TIGHT::_) => "tightcode"
		  | _ => "centercode"
		(* end case *))
	  in
	    OF{
		name = name,
		codeEnv = codeEnv,
		closed = ref false,
		enabled = ref false,
		text = ref []
	      }
	  end

    fun nameOf (OF{name, ...}) = name

    fun sameFile (OF{name=a, ...}, OF{name=b, ...}) = Atom.sameAtom(a, b)

    fun enable (OF{enabled, closed=ref false, ...}) = (enabled := true)
      | enable _ = ()
    fun disable (OF{enabled, ...}) = (enabled := false)

    fun output outFn (OF{enabled=ref true, text, ...}) = text := outFn(!text)
      | output _ _ = ()

    fun highlight true f = output (fn l => HLIGHT_ON::l) f
      | highlight false f = output (fn l => HLIGHT_OFF::l) f

    fun mbox f = output (fn l => MBOX::l) f

    fun text s f = output (fn l => (RAW s)::l) f

    fun line ln f = output (fn l => (LN ln)::l) f

    fun close (OF{closed = ref true, ...}) = ()
      | close (OF{name, text, codeEnv, closed, enabled, ...}) = let
	  val outStrm = TextIO.getOutstream(TextIO.openOut (Atom.toString name))
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
	  fun putText (hlight, []) = ()
	    | putText (hlight, elem::r) = putText (
		case elem
		 of (RAW s) => (pr s; hlight)
		  | MBOX => (pr "\\mbox{}\n"; hlight)
		  | (LN toks) => (
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
		  | HLIGHT_ON => true
		  | HLIGHT_OFF => false
		(* end case *),
		r)
	  in
	    pr (concat["\\begin{", codeEnv, "}\n"]);
	    putText (false, outdent (!text));
	    pr (concat["\\end{", codeEnv, "}\n"]);
	    TextIO.StreamIO.closeOut outStrm;
	    text := [];
	    closed := true;
	    enabled := false
	  end

  end;

