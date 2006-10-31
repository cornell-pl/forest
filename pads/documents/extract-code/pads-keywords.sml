structure PadsKeywords : sig

    val mkToken : {space : int, text : string}
	  -> {space : int, kind : Tokens.token_kind, text : string}

  end = struct

    structure T = Tokens

  (* the keyword hash table *)
    exception Keyword
    val keywords : T.token_kind AtomTable.hash_table = AtomTable.mkTable(64, Keyword)

  (* insert the reserved words into the keyword hash table *)
    val _ = let
	  val insert = AtomTable.insert keywords
	  fun ins (s, item) = insert (Atom.atom s, item)
	  in
	    app ins [
(*  comment out if don't want c keywords in bold *)
              ("break", T.Keyword),
	      ("case", T.Keyword),
	      ("continue", T.Keyword),
	      ("default", T.Keyword),
	      ("do", T.Keyword),
	      ("else", T.Keyword),
	      ("auto", T.Keyword),
	      ("extern", T.Keyword),
	      ("register", T.Keyword),
	      ("static", T.Keyword),
	      ("for", T.Keyword),
	      ("goto", T.Keyword),
	      ("if", T.Keyword),
	      ("char", T.Keyword),
	      ("double", T.Keyword),
	      ("enum", T.Keyword),
	      ("float", T.Keyword),
	      ("int", T.Keyword),
	      ("long", T.Keyword),
	      ("short", T.Keyword),
	      ("struct", T.Keyword),
	      ("union", T.Keyword),
	      ("unsigned", T.Keyword),
	      ("signed", T.Keyword),
	      ("volatile", T.Keyword),
	      ("void", T.Keyword),
	      ("sizeof", T.Keyword),
	      ("typedef", T.Keyword),
	      ("return", T.Keyword),
	      ("switch", T.Keyword),
	      ("while", T.Keyword),
 	      ("const", T.Keyword),

              (* PADS KEYWORDS *)
              ("Parray", T.Keyword),
              ("Palternates", T.Keyword),
              ("Pcase",T.Keyword),
              ("Pcharclass",T.Keyword),
              ("Pcompute",T.Keyword),
              ("Pdefault",T.Keyword),
              ("Pended",T.Keyword),
              ("Pendian",T.Keyword),
              ("Penum", T.Keyword),
              ("Peor", T.Keyword),
              ("Pforall", T.Keyword),
              ("Pfrom", T.Keyword),
              ("Pin", T.Keyword),
              ("Pinclude", T.Keyword),
              ("Plast", T.Keyword),
              ("Plongest", T.Keyword),
              ("Pmaskmap", T.Keyword),
              ("Pnone", T.Keyword),
              ("Pnosep", T.Keyword),
              ("Pomit", T.Keyword),
              ("Popt", T.Keyword),
              ("Pparsecheck", T.Keyword),
              ("Pprefix", T.Keyword),
              ("Ppre", T.Keyword),
              ("Precord", T.Keyword),
              ("Precur", T.Keyword),
              ("Pselect", T.Keyword),
              ("Psep", T.Keyword),
              ("Psource", T.Keyword),
              ("Psome", T.Keyword),
              ("Pstruct", T.Keyword),
              ("Pswitch",T.Keyword),
              ("Pterm", T.Keyword),
              ("Ptrans", T.Keyword),
              ("Ptry", T.Keyword),
              ("Ptypedef", T.Keyword),
              ("Punion",T.Keyword),
              ("Pwhere",T.Keyword)
			  ]
	  end

    val peek = AtomTable.find keywords

    fun mkToken {space, text} = let
	  val name = Atom.atom text
	  val kind = (case (peek name) of (SOME k) => k | _ => T.Ident)
	  in

	    {space = space, kind = kind, text = Atom.toString name}
	  end

  end (* MobyKeywords *)
