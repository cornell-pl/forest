(* main.sml
 *
 * The main module for extract-code.
 *
 * A filter for extracting TeX'able code from SML source files.
 * See the README file for a description of the commands.
 *)

structure Main : sig

    val main : (string * string list) -> OS.Process.status

  end = struct

    structure O = OutFile
    structure T = Tokens
    structure SS = Substring

  (* language definitions *)
    structure SML_FE = ScannerFn (
      val name = "sml"
      val exts = ["sml", "sig"]
      structure Lexer = SMLLexer)
    structure PADS_FE = ScannerFn (
      val name = "pads"
      val exts = ["p"]
      structure Lexer = PadsLexer)

    type file_set = O.outfile list

  (* return (fs1 ^ fs2, fs1 - fs2) *)
    fun splitFileSet (fs1, fs2) = let
	  fun split ([] : file_set, inter, diff) = (inter, diff)
	    | split (f::r, inter, diff) = let
		fun inSet ([] : file_set) = false
		  | inSet (f'::r) = O.sameFile(f, f') orelse (inSet r)
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

    fun doFile (lang, srcFName) = (let
	  val {get=getLn, error=err} =
		valOf (Languages.makeScanner {lang=lang, file=srcFName})
	  val date = getDate()
	  fun initFile file = let
		val dstFName = Atom.toString(O.nameOf file)
		in
		  TextIO.output(TextIO.stdErr, String.concat[
		      "extracting ", dstFName, " from ", srcFName, "\n"
		    ]);
		  O.text (String.concat["% ", dstFName, "\n"]) file;
		  O.text (String.concat["% extracted from ", srcFName, "\n"]) file;
		  O.text (String.concat["% ", date, "\n"]) file;
		  O.text "%\n" file
		end
	  fun error s = (err s; raise Fail s)
	  val fileTbl = AtomTable.mkTable (8, Fail "FileTbl")
	  fun mkFile attrs fname = (case (AtomTable.find fileTbl fname)
		 of NONE => let
		      val f = O.openFile (fname, attrs)
		      in
			AtomTable.insert fileTbl (fname, f); f
		      end
		  | (SOME _) => error "duplicate file name"
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
		      then app O.mbox curFiles
		      else app (O.line l) curFiles
		in
(**
print "doCmd: "; prFiles curFiles; print "\n";
**)
		  case cmd
		   of T.Eof => (
			appAllFiles O.close;
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
				app O.enable fs;
				nextCmd (ifStk, curFiles@fs)
			      end
			  | _ => error "unexpected BEGIN"
			(* end case *))
		    | (T.END[]) => (
			app O.disable curFiles;
			nextCmd (ifStk, []))
		    | (T.END files) => (case ifStk
			 of [] => let
			      val fs = mkFileSet files
			      val (_, curFiles) = splitFileSet(curFiles, fs)
			      in
				app O.disable fs;
				nextCmd (ifStk, curFiles)
			      end
			  | _ => error "unexpected END"
			(* end case *))
		    | T.BEGIN_ELLIPSE => (
			app O.disable curFiles;
			continue())
		    | T.END_ELLIPSE => (
			app O.enable curFiles;
			continue())
		    | T.BEGIN_HIGHLIGHT => (
			app (O.highlight true) curFiles;
			continue())
		    | T.END_HIGHLIGHT => (
			app (O.highlight false) curFiles;
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
	    handle ex => TextIO.output(TextIO.stdErr, String.concat[
		"extract-code: ", exnMessage ex, "\n"
	      ]))

    fun stripFile (lang, srcFName) = (let
	  val {get=getLn, error=err} =
		valOf (Languages.makeScanner {lang=lang, file=srcFName})
	  val outStrm = TextIO.openOut(OS.Path.joinBaseExt{
		  base=srcFName, ext=SOME "strip"
		})
	  val outS = TextIO.getOutstream outStrm
	  fun tabs pos = let val w = Word.fromInt pos
		in
		(* (pos div 8, pos mod 8) *)
		  (Word.toIntX(Word.>>(w, 0w3)), Word.toIntX(Word.andb(w, 0w7)))
		end
	  fun outputSp (p, n) = let
		fun repeat (c, 0) = ()
		  | repeat (c, n) = (
		      TextIO.StreamIO.output1(outS, c); repeat(c, n-1))
		val (t, s) = tabs p
		val (t', s') = tabs(p+n)
		in
		  if (t' > t)
		    then (repeat (#"\t", t'-t); repeat(#" ", s'))
		    else repeat (#" ", s'-s)
		end
	  fun output ln = let
		fun outToken ({space, text, kind}, pos) = (
		      outputSp (pos, space);
		      TextIO.StreamIO.output(outS, text);
		      pos+space+size text)
		in
		  ignore(List.foldl outToken 0 ln);
		  TextIO.StreamIO.output1(outS, #"\n")
		end
	  fun strip () = (case getLn()
		 of T.Eof => ()
		  | T.Error => strip()
		  | T.Text ln => (output ln; strip())
		  | _ => strip()
		(* end case *))
	  in
	    TextIO.output(TextIO.stdErr, String.concat[
		"stripping ", srcFName, "\n"
	      ]);
	    strip ();
	    TextIO.closeOut outStrm
	  end
	    handle ex => TextIO.output(TextIO.stdErr, String.concat[
		"extract-code: ", exnMessage ex, "\n"
	      ]))

(*
    fun main' ("-strip"::r) = stripFiles r
      | main' [] = doFile ("<stdin>", TextIO.stdIn)
      | main' files = let
	  fun openF fname = (let
		val instrm = TextIO.openIn fname
		in
		  doFile (fname, instrm);
		  TextIO.closeIn instrm
		end
		  handle ex => TextIO.output(TextIO.stdErr, String.concat[
		      "extract-code: ", exnMessage ex, "\n"
		    ]))
	  in
	    app openF files
	  end
*)

    fun doArgs (lang, strip, []) = ()
      | doArgs (_, strip, "-lang"::lang::r) = doArgs(SOME lang, strip, r)
      | doArgs (lang, strip, "-strip"::r) = doArgs(lang, true, r)
      | doArgs (_, _, ["-lang"]) = TextIO.output(TextIO.stdErr,
	  "extract-code: missing argument to \"-lang\" option\n")
      | doArgs (lang, true, file::r) = (
	  stripFile(lang, file);
	  doArgs (lang, true, r))
      | doArgs (lang, false, file::r) = (
	  doFile(lang, file);
	  doArgs (lang, false, r))

    fun main (_, args) = (doArgs (NONE, false, args); OS.Process.success)

  end; (* Main *)
