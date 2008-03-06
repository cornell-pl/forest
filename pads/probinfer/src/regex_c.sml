structure Regex_c = struct

    open Basetokens

    fun match (str, btoken, re) = 
      let
val _ = print ("Match string (" ^ (String.toString str) ^") with token ("^(BTokenToName btoken) ^ ")\n") 
        val s1' = ZString.dupML' str
        val s2' = ZString.dupML' re
        val s =     C.Cvt.ml_sint (C.call (F_get_start.fptr (),
                  (s1', s2')))
        val s = MLRep.Signed.toInt s
      in 
        if s = ~1 then (C.free' s1'; C.free' s2'; NONE)
        else if s = ~2 then (print "this token re is wrong.\n"; C.free' s1'; C.free' s2'; NONE)
        else
          let
            val l =     C.Cvt.ml_sint (C.call (F_get_len.fptr (),
                  (s1', s2')))
            val l = MLRep.Signed.toInt l
            val _ = (C.free' s1'; C.free' s2')
            val _ = print ("find a match starting at pos "^(Int.toString s)^" ")
            val _ = print ("of length "^(Int.toString l)^"\n")
          in
            SOME (s, l)
          end
      end

end
