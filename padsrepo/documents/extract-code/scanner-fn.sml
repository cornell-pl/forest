(* scanner-fn.sml
 *
 * The input engine for extract-code.
 *)

signature LANGUAGE =
  sig
    val name : string
    val exts : string list
    structure Lexer : sig
	structure UserDeclarations : sig
	    datatype lexresult
	      = EOF
	      | NL
	      | TOK of {space : int, kind : Tokens.token_kind, text : string}
	      | INSERT_BEGIN
	      | INSERT_END
	      | CMD of (string * string)
	      | COM of lexresult list
	      | STR of lexresult list
	  end
	val makeLexer : (int -> string) -> unit -> UserDeclarations.lexresult
      end
  end

functor ScannerFn (L : LANGUAGE) : sig end =
  struct

    structure F = Format
    structure U = L.Lexer.UserDeclarations
    structure T = Tokens

    val errorMsg = F.format "Error [\"%s\", %d]: %s\n"

(** NOTE: we need to do something about the line numbers and
 ** error reporting!
 **)
    fun makeScanner (fname, instrm) = let
	  val lnum = ref 0
	  fun input _ = (TextIO.inputAll instrm before TextIO.closeIn instrm)
	  val lexer = L.Lexer.makeLexer input
	  fun error s = TextIO.output(TextIO.stdErr,
		errorMsg [F.STR fname, F.INT(!lnum), F.STR s])
	  val buf = ref NONE
	  fun getLine ([], U.EOF) = T.Eof
	    | getLine (l, U.EOF) = (buf := SOME U.EOF; T.Text(rev l))
	    | getLine (l, U.NL) = (lnum := !lnum+1; T.Text(rev l))
	    | getLine (l, U.TOK tok) = next(tok :: l)
	    | getLine (_, U.CMD cmd) = T.matchCmd cmd
	    | getLine ([], U.INSERT_BEGIN) = getInsert([], lexer())
	    | getLine (l, U.COM[]) = next l
	    | getLine (l, U.COM[U.TOK tok]) = next (tok :: l)
	    | getLine (l, U.COM(U.TOK tok :: U.NL :: r)) = (
		buf := SOME(U.COM r);
		T.Text(rev (tok::l)))
	    | getLine (l, U.STR[]) = next l
	    | getLine (l, U.STR[U.TOK tok]) = next (tok :: l)
	    | getLine (l, U.STR(U.TOK tok :: U.NL :: r)) = (
		buf := SOME(U.STR r);
		T.Text(rev (tok::l)))
	  and next l = getLine (l, lexer())
	  and getInsert (lns, U.INSERT_END) = T.INSERT(rev lns)
	    | getInsert (lns, tok) = let
		fun getLn (l, U.NL) = (
		      lnum := !lnum+1;
		      getInsert ((rev l) :: lns, lexer()))
		  | getLn (l, U.TOK tok) = next (tok::l)
		  | getLn (l, U.COM[]) = next l
		  | getLn (l, U.COM[U.TOK tok]) = next(tok :: l)
		  | getLn (l, U.COM(U.TOK tok :: U.NL :: r)) =
		      getInsert (rev (tok::l) :: lns, U.COM r)
		  | getLn (l, U.STR[]) = next l
		  | getLn (l, U.STR[U.TOK tok]) = next (tok :: l)
		  | getLn (l, U.STR(U.TOK tok :: U.NL :: r)) =
		      getInsert (rev (tok::l) :: lns, U.STR r)
		and next l = getLn (l, lexer())
		in
		  getLn ([], tok)
		end
	  fun get () = (case !buf
		 of NONE => getLine([], lexer())
		  | (SOME stuff) => (buf := NONE; getLine([], stuff))
		(* end case *))
	  in
	    {get = get, error = error}
	  end

  (* initialization *)
    val _ = Languages.register {
	  name = L.name,
	  exts = L.exts,
	  makeScanner = makeScanner
	}

  end; (* Scanner *)
