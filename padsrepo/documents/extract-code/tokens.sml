(* tokens.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

structure Tokens : sig

  (* the different kinds of displayed objects *)
    datatype token_kind
      = Comment
      | Keyword
      | Symbol
      | Delim
      | Ident
      | TyVar
      | Literal

    type line = {space:int, kind:token_kind, text:string} list

    val extent : line -> {indent : int, len : int}
	(* returns the amount of leading white space and the total line
	 * length.
	 *)

    datatype attr
      = LEFT
      | TIGHT

    datatype cmd
      = Eof
      | Error
      | Text of line
      | FILE of (attr list * Atom.atom list)
      | BEGIN of Atom.atom list
      | END of Atom.atom list
      | BEGIN_ELLIPSE
      | END_ELLIPSE
      | BEGIN_HIGHLIGHT
      | END_HIGHLIGHT
      | IF_FILE of Atom.atom list
      | ELIF_FILE of Atom.atom list
      | ELSE
      | END_IF
      | INSERT of line list

    val matchCmd : (string * string) -> cmd

    val toString : {space:int, kind:token_kind, text:string} -> string

  end = struct

    structure SS = Substring

  (* the different kinds of displayed objects *)
    datatype token_kind
      = Comment
      | Keyword
      | Symbol
      | Delim
      | Ident
      | TyVar
      | Literal

    type line = {space:int, kind:token_kind, text:string} list

  (* returns the amount of leading white space and the total line
   * length.
   *)
    fun extent [] = {indent=0, len=0}
      | extent (l as ({space, kind, text}::_)) = let
	  val n = List.foldl
		(fn ({space, text, ...}, sum) => sum + space + size text) 0 l
	  in
	    {indent=space, len=n}
	  end

    datatype attr
      = LEFT
      | TIGHT

    datatype cmd
      = Eof
      | Error
      | Text of line
      | FILE of (attr list * Atom.atom list)
      | BEGIN of Atom.atom list
      | END of Atom.atom list
      | BEGIN_ELLIPSE
      | END_ELLIPSE
      | BEGIN_HIGHLIGHT
      | END_HIGHLIGHT
      | IF_FILE of Atom.atom list
      | ELIF_FILE of Atom.atom list
      | ELSE
      | END_IF
      | INSERT of line list


  (* this is a list of all of the commands, except INSERT, which must be
   * treated specially in the lexer.
   *)
    datatype cmd_fmt
      = CMD of cmd
      | CMD_FILES of (Atom.atom list -> cmd)
      | CMD_ATTRS_FILES of (attr list * Atom.atom list -> cmd)
    val cmdList = [
	    ("FILE",		CMD_ATTRS_FILES FILE),
	    ("BEGIN-ELLIPSE",	CMD BEGIN_ELLIPSE),
	    ("END-ELLIPSE",	CMD END_ELLIPSE),
	    ("BEGIN-HIGHLIGHT",	CMD BEGIN_HIGHLIGHT),
	    ("END-HIGHLIGHT",	CMD END_HIGHLIGHT),
	    ("BEGIN",		CMD_FILES BEGIN),
	    ("END",		CMD_FILES END),
	    ("IF-FILE",		CMD_FILES IF_FILE),
	    ("ELIF-FILE",	CMD_FILES ELIF_FILE),
	    ("ELSE",		CMD ELSE),
	    ("END-IF",		CMD END_IF)
	  ]
    val attrList = [
	    (Atom.atom "@LEFT",		LEFT),
	    (Atom.atom "@TIGHT",	TIGHT)
	  ]

    fun matchCmd (cmdName, rest) = (
	  case List.find (fn (x, _) => (x = cmdName)) cmdList
	   of (SOME(_, CMD cmd)) => cmd
	    | (SOME(_, CMD_FILES cmd)) => let
		val ss = (SS.full rest)
		fun cvt f = Atom.atom(SS.string f)
		in
		  cmd (List.map cvt (SS.tokens (Char.contains " \t\n,") ss))
		end
	    | (SOME(_, CMD_ATTRS_FILES cmd)) => let
		val ss = (SS.full rest)
		fun cvt f = Atom.atom(SS.string f)
		fun split ([], attrs) = (List.rev attrs, [])
		  | split (tok::r, attrs) = (
		      case (List.find (fn (x, _) => Atom.sameAtom(tok, x)) attrList)
		       of (SOME(_, a)) => split (r, a::attrs)
			| NONE => (rev attrs, tok::r)
		      (* end case *))
		val toks = List.map cvt (SS.tokens (Char.contains " \t\n,") ss)
		in
		  cmd (split (toks, []))
		end
	    | NONE => Error
	  (* end case *))

    fun toString {space, kind, text} = let
	  val kind' = (case kind
		 of Comment => "Comment"
		  | Keyword => "Keyword"
		  | Symbol => "Symbol"
		  | Delim => "Delim"
		  | Ident => "Ident"
		  | TyVar => "TyVar"
		  | Literal => "Literal"
		(* end case *))
	  in
	    concat[
		kind', "(sp=", Int.toString space, ", txt=\"",
		String.toString text, "\""
	      ]
	  end

  end (* Tokens *)
