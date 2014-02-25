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

    structure T = Tokens
    structure SS = Substring

  (* language definitions *)
    structure SML_FE = ScannerFn (
      val name = "sml"
      val exts = ["sml", "sig"]
      structure Lexer = SMLLexer)
    structure PADS_FE = ScannerFn (
      val name = "pads"
      val exts = ["p", "c", "h"]
      structure Lexer = PadsLexer)

  (* output target types *)
    datatype target = TEX | HVA | SRC

  (* report an uncaught exception *)
    fun uncaughtExn (srcFile, exn) = (
	  TextIO.output(TextIO.stdErr, concat[
	      "[", srcFile, "]: ", General.exnMessage exn, "\n"
	    ]);
	  app (fn s => TextIO.output(TextIO.stdErr, concat[
	      "  raised at ", s, "\n"
	    ]))
	    (SMLofNJ.exnHistory exn);
	  OS.Process.exit OS.Process.failure)

    fun stripFile (lang, srcFName) = let
	  val SOME{get=getLn, error=err} =
		Languages.makeScanner {lang=lang, file=srcFName}
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

    fun doFile (target, lang, file) = (
	  case target
	   of TEX => TeXOutput.doFile {lang=lang, srcFName=file}
	    | HVA => HeVeAOutput.doFile {lang=lang, srcFName=file}
	    | SRC => stripFile (lang, file)
	  (* end case *))
	    handle ex => uncaughtExn (file, ex)

    fun doArgs (lang, target, []) = ()
      | doArgs (_, target, "-lang"::lang::r) = doArgs(SOME lang, target, r)
      | doArgs (lang, _, "-strip"::r) = doArgs(lang, SRC, r)
      | doArgs (lang, _, "-hevea"::r) = doArgs(lang, HVA, r)
      | doArgs (lang, _, "-tex"::r) = doArgs(lang, TEX, r)
      | doArgs (_, _, ["-lang"]) = TextIO.output(TextIO.stdErr,
	  "extract-code: missing argument to \"-lang\" option\n")
      | doArgs (lang, target, file::r) = (
	  doFile (target, lang, file);
	  doArgs (lang, target, r))

    fun main (_, args) = (doArgs (NONE, TEX, args); OS.Process.success)

  end; (* Main *)
