structure Utils =
struct
    fun doTranspose m =
	let fun transposeOne (dc, acc) = 
	    let fun tO' (dc,acc) result = 
		case (dc,acc) 
		    of ([], [])          => List.rev result
		  |  (nt::tks, na::accs) => tO' (tks, accs) ((nt :: na)::result)
		  | _                    => raise Fail "UnequalLengths"
	    in
		tO' (dc,acc) []
	    end
	    fun listify ls = List.map (fn x => [x]) ls 
	    val revTrans = case m
		of [] => []
	      |  [dc] => listify dc
	      |  (dc::rest) => List.foldl transposeOne (listify dc) rest 
	in
	    List.map List.rev revTrans
	end

    fun lconcat ls = 
	let fun doit l a = 
	    case l of [] => a
            | (s::ss) => doit ss (s^a)
	in
	    doit (List.rev ls) ""
	end
   
    (*funtion to join a list of strings with a string token*)
    fun join ls s =
	let fun doit l a = 
	    case l of 
	     nil => ""
	    | (s::nil)=> s
            | (s::ss) => (s^a) ^ (doit ss a)
	in
	    doit ls s
	end

    (* position in a list *)
    fun position ( a : ''a ) ( l : ''a list ): int option =
    let fun position' ( n : int ) ( a : ''a ) ( l : ''a list ): int option =
        ( case l of
               []      => NONE
             | (x::xs) => if x = a then SOME n else position' (n+1) a xs
        )
    in position' 0 a l
    end

   (* replace reserved char in a string with escapes for use in PADS re *)
   fun escape s =
     let
	fun escapeChar (c:char) : string = case c of
	  #"?" => "\\\\?"
	| #"*" => "\\\\*"
	| #"+" => "\\\\+"
	| #"|" => "\\\\|"
	| #"$" => "\\\\$"
	| #"^" => "\\\\^"
	| #";" => "\\\\;"
	| #"." => "\\\\."
	| #"=" => "\\\\="
	| #"-" => "\\\\-"
	| #"/" => "\\\\/"
	| #"\\" => "\\\\\\"
  	| #"(" => "\\\\("
  	| #")" => "\\\\)"
  	| #"[" => "\\\\["
  	| #"]" => "\\\\]"
  	| #"<" => "\\\\<"
  	| #">" => "\\\\>"
  	| #"{" => "\\\\{"
  	| #"}" => "\\\\}"
	| _ => Char.toString c
     in
        String.translate escapeChar s
     end

    (* this function checks if a string is a valid C identifier *)
    fun isCIdentifier s =
      let
	fun isWordChar c = (Char.isAlphaNum c) orelse (c = #"_")
	fun isWord s = if isWordChar (String.sub (s, 0)) then 
			if size s = 1 then true
			else isWord (String.extract (s, 1, NONE))
		       else false
      in
	if Char.isAlpha (String.sub (s, 0)) then 
		if size s = 1 then true
		else isWord (String.extract (s, 1, NONE))
	else false
      end

     fun sumInts ( ns : int list ) : LargeInt.int = 
         foldl ( fn ( x : int, y : LargeInt.int ) => y + Int.toLarge x ) 0 ns

     fun avgInts ( ns : int list ) : real = 
        ( Real.fromLargeInt ( sumInts ns ) ) / ( Real.fromInt ( length ns ) )

end
