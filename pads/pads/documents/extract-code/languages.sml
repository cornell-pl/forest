(* languages.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies.
 *
 *)

structure Languages : sig

    type scanner = {
	get : unit -> Tokens.cmd,
	error : string -> unit
      }

    type language_spec = {
	name : string,
	exts : string list,		(* file-name extensions (w/o the ".") *)
	makeScanner : (string * TextIO.instream) -> scanner
      }

    val register : language_spec -> unit

    val makeScanner : {lang : string option, file : string} -> scanner option

  end = struct

    type scanner = {
	get : unit -> Tokens.cmd,
	error : string -> unit
      }

    type language_spec = {
	name : string,
	exts : string list,		(* file-name extensions (w/o the ".") *)
	makeScanner : (string * TextIO.instream) -> scanner
      }

    val specList = ref([] : language_spec list)

    fun register spec = specList := spec :: !specList

    fun matchLang lang (spec : language_spec) = (#name spec = lang)
    fun matchFile file = (case OS.Path.splitBaseExt file
	   of {ext = NONE, ...} => (fn _ => false)
	    | {ext = SOME ext, ...} => let
		fun matchExt (spec : language_spec) =
		      (List.exists (fn e => e = ext) (#exts spec))
		in
		  matchExt
		end
	  (* end case *))

    fun findLang {lang = SOME lang, ...} = List.find (matchLang lang) (!specList)
      | findLang {lang, file} = List.find (matchFile file) (!specList)

    fun makeScanner {lang, file} = (case findLang {file=file, lang=lang}
	   of NONE => NONE
	    | SOME{makeScanner, ...} => (let
		val inStrm = TextIO.openIn file
		in
		  SOME(makeScanner(file, inStrm))
		end
		  handle _ => NONE)
	  (* end case *))

  end

