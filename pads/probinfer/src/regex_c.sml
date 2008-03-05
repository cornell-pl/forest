structure Regex_c = struct
    fun match (s1, s2) = let
    val s1' = ZString.dupML' s1
    val s2' = ZString.dupML' s2
    val s =     C.Cvt.ml_sint (C.call (F_get_start.fptr (),
                  (s1', s2')))
    val l =     C.Cvt.ml_sint (C.call (F_get_len.fptr (),
                  (s1', s2')))
    val _ = (C.free' s1'; C.free' s2')
    val _ = print ((Int.toString (MLRep.Signed.toInt s))^"\n")
    val _ = print ((Int.toString (MLRep.Signed.toInt l))^"\n")
    in
      (s, l)
    end

end
