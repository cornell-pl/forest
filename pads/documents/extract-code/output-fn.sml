(* output-fn.sml
 *
 * COPYRIGHT (c) 2002 John Reppy (jhr@cs.uchicago.edu)
 *)

signature OUTPUT_DEVICE =
  sig

    val suffix : string

    val output : {
	    outStrm : TextIO.StreamIO.outstream,
	    codeEnv : string,
	    text : OutputReps.text list
	  } -> unit

  end

signature OUTPUT =
  sig

    val doFile : {lang : string option, srcFName : string} -> unit

  end

functor OutputFn (Dev : OUTPUT_DEVICE) : OUTPUT =
  struct

    structure SS = Substring
    structure T = Tokens

    datatype text = datatype OutputReps.text

    datatype outfile = datatype OutputReps.outfile

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

    fun text s f = output (fn l => (RAW s)::l) f

    fun comment (OF{text, ...}, s) = (text := COMMENT s :: !text)

    fun line ln f = output (fn l => (LN ln)::l) f

    fun close (OF{closed = ref true, ...}) = ()
      | close (OF{name, text, codeEnv, closed, enabled, ...}) = let
	  val outStrm = TextIO.getOutstream(TextIO.openOut (Atom.toString name))
	  in
	    Dev.output {outStrm = outStrm, codeEnv = codeEnv, text = !text};
	    TextIO.StreamIO.closeOut outStrm;
	    text := [];
	    closed := true;
	    enabled := false
	  end

    type file_set = outfile list

  (* return (fs1 ^ fs2, fs1 - fs2) *)
    fun splitFileSet (fs1, fs2) = let
	  fun split ([] : file_set, inter, diff) = (inter, diff)
	    | split (f::r, inter, diff) = let
		fun inSet ([] : file_set) = false
		  | inSet (f'::r) = sameFile(f, f') orelse (inSet r)
		in
		  if (inSet fs2)
		    then split(r, f::inter, diff)
		    else split(r, inter, f::diff)
		end
	  in
	    split (fs1, [], [])
	  end

  (* values in the stack of IF statements *)
    datatype if_stk_item
      = ELSE of (file_set * file_set)
      | ENDIF of file_set

    fun getDate () = Date.toString(Date.fromTimeLocal(Time.now()))

    fun isBlank [] = true
      | isBlank l = let
	  fun isB {space, kind, text} =
		SS.isEmpty(SS.dropl Char.isSpace (SS.all text))
	  in
	    List.all isB l
	  end

    fun doFile {lang, srcFName} = let
	  val SOME{get=getLn, error=err} =
		Languages.makeScanner {lang=lang, file=srcFName}
	  val date = getDate()
	  fun initFile file = let
		val dstFName = Atom.toString(nameOf file)
		in
		  TextIO.output(TextIO.stdErr, String.concat[
		      "extracting ", dstFName, " from ", srcFName, "\n"
		    ]);
		  comment (file, dstFName);
		  comment (file, "extracted from " ^ srcFName);
		  comment (file, date);
		  comment (file, "")
		end
	  fun error s = (err s; raise Fail s)
	  val fileTbl = AtomTable.mkTable (8, Fail "FileTbl")
	  fun mkFile attrs fname = (case (AtomTable.find fileTbl fname)
		 of NONE => let
		      val fname' = Atom.toString fname
		      val fname'' = Atom.atom(OS.Path.joinBaseExt(
			    case OS.Path.splitBaseExt fname'
			     of {base, ext=SOME "tex"} =>
				  {base = base, ext = SOME Dev.suffix}
			      | _ => {base = fname', ext = SOME Dev.suffix}
			    (* end case *)))
		      val f = openFile (fname'', attrs)
		      in
			AtomTable.insert fileTbl (fname, f); f
		      end
		  | (SOME _) => error(concat[
			  "duplicate file name \"", Atom.toString fname, "\""
			])
		(* end case *))
	  fun mkFileSet files = let
		fun mkFile s = (case (AtomTable.find fileTbl s)
		       of (SOME f) => f
			| NONE => error(concat[
			      "unopened file \"", Atom.toString s, "\""
			    ])
		      (* end case *))
		in
		  map mkFile files
		end
	  fun appAllFiles f = AtomTable.app f fileTbl
	  fun doCmd (cmd, ifStk, curFiles) = let
		fun continue () = nextCmd(ifStk, curFiles)
		fun doLine l = if (isBlank l)
		      then app (line []) curFiles
		      else app (line l) curFiles
		in
(**
print "doCmd: "; prFiles curFiles; print "\n";
**)
		  case cmd
		   of T.Eof => (
			appAllFiles close;
			case (ifStk, curFiles)
			 of ([], []) => ()
			  | _ => error "unexpected EOF"
			(* end case *))
		    | T.Error => continue()
		    | (T.Text l) => (doLine l; continue())
		    | (T.FILE(attrs, files)) => (case ifStk
			 of [] => let
			    val newFiles = map (mkFile attrs) files
			    in
			      app initFile newFiles;
			      nextCmd(ifStk, curFiles)
			    end
			  | _ => error "unexpected FILE"
			(* end case *))
		    | (T.BEGIN[]) => error "missing files in BEGIN"
		    | (T.BEGIN files) => (case ifStk
			 of [] => let
			      val fs = mkFileSet files
			      val (_, curFiles) = splitFileSet(curFiles, fs)
			      in
				app enable fs;
				nextCmd (ifStk, curFiles@fs)
			      end
			  | _ => error "unexpected BEGIN"
			(* end case *))
		    | (T.END[]) => (
			app disable curFiles;
			nextCmd (ifStk, []))
		    | (T.END files) => (case ifStk
			 of [] => let
			      val fs = mkFileSet files
			      val (_, curFiles) = splitFileSet(curFiles, fs)
			      in
				app disable fs;
				nextCmd (ifStk, curFiles)
			      end
			  | _ => error "unexpected END"
			(* end case *))
		    | T.BEGIN_ELLIPSE => (
			app disable curFiles;
			continue())
		    | T.END_ELLIPSE => (
			app enable curFiles;
			continue())
		    | T.BEGIN_HIGHLIGHT => (
			app (highlight true) curFiles;
			continue())
		    | T.END_HIGHLIGHT => (
			app (highlight false) curFiles;
			continue())
		    | (T.IF_FILE files) => let
			val fs = mkFileSet files
			val (thenFiles, elseFiles) = splitFileSet(curFiles, fs)
			val ifStk' = ELSE(elseFiles, curFiles) :: ifStk
			in
(***
print "IF-FILE\n";
***)
			  nextCmd (ifStk', thenFiles)
			end
		    | (T.ELIF_FILE files) => (case ifStk
			 of (ELSE(elseFiles, allFiles)::r) => let
			      val fs = mkFileSet files
			      val (thenFiles, elseFiles) =
				    splitFileSet(elseFiles, fs)
			      val ifStk' = ELSE(elseFiles, allFiles) :: r
			      in
(***
print "ELIF-FILE\n";
***)
				nextCmd (ifStk', thenFiles)
			      end
			  | _ => error "unexpected ELIF-FILE"
			(* end case *))
		    | T.ELSE => (case ifStk
			 of (ELSE(elseFiles, allFiles)::r) =>
(***
(print "ELSE\n";
***)
			      nextCmd ((ENDIF allFiles)::r, elseFiles)
(***
)
***)
			  | _ => error "unexpected ELSE"
			(* end case *))
		    | T.END_IF => (case ifStk
			 of (ELSE(_, allFiles)::r) => nextCmd (r, allFiles) 
			  | ((ENDIF allFiles)::r) => nextCmd (r, allFiles)
			  | [] => error "unexpected END-IF"
			(* end case *))
		    | (T.INSERT stuff) => (app doLine stuff; continue())
		  (* end case *)
		end
	  and nextCmd (ifStk, curFiles) =
		doCmd(getLn(), ifStk, curFiles)
	  in
	    nextCmd ([], [])
	  end

  end
