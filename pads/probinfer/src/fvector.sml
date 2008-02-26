structure Fvector = 
struct

  open Config

(* feature vector (low-high): upper-case? lower-case? digit? punc? whitespace? "."? ","? """? ":"? "/"? *)
    fun charToList c : int list = 
      let
        val upper = if Char.isUpper c then [1] else [0]
        val lower = if Char.isLower c then 1::upper else 0::upper
        val digit = if Char.isDigit c then 1::lower else 0::lower
        val punct = if Char.isPunct c orelse Char.isSpace c then 1::digit else 0::digit
        val space = if Char.isSpace c then 1::punct else 0::punct
        val dot = if Char.compare(c, #".")=EQUAL then 1::space else 0::space
        val comma = if Char.compare(c, #",")=EQUAL then 1::dot else 0::dot
        val quest = if Char.compare(c, #"?")=EQUAL then 1::comma else 0::comma
        val quote = if Char.compare(c, #"\"")=EQUAL then 1::quest else 0::quest
        val colon = if Char.compare(c, #":")=EQUAL then 1::quote else 0::quote
        val slash = if Char.compare(c, #"/")=EQUAL then 1::colon else 0::colon
      in
        slash
      end

    exception WrongFeatureVector


    fun printList i = print (Int.toString i)

    fun listToInt l =
      case l of 
          [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1] => 0 (* upper *)
        | [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0] => 1 (* lower *)
        | [0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0] => 2 (* digit *)
        | [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0] => 3 (* other punct *)
        | [0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0] => 4 (* space *)
        | [0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0] => 5 (* dot *)
        | [0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0] => 6 (* comma *)
        | [0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0] => 7 (* question *)
        | [0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0] => 8 (* quote *)
        | [0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0] => 9 (* colon *)
        | [1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0] => 10 (*slash *)
        | _ => raise WrongFeatureVector  

   exception WrongIntForFVector

   fun intToList i =
      case i of
          0 => [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1] 
        | 1 => [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0] 
        | 2 => [0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0] 
        | 3 => [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0] 
        | 4 => [0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0] 
        | 5 => [0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0] 
        | 6 => [0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0] 
        | 7 => [0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0] 
        | 8 => [0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0] 
        | 9 => [0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0] 
        | 10 => [1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0] 
        | _ => raise WrongIntForFVector

    val maxFOrd = 10

(*
    fun intToList i =
      let
        fun recFn bit = 
          if bit=fvectorbits then ([], i) 
          else
            let
              val (retlist, remain) = recFn (bit+1)
              val powv = Real.toInt IEEEReal.TO_NEAREST (Math.pow((Real.fromInt 2), (Real.fromInt bit)))
              val mybit = if remain-powv>=0 then 1 else 0
              val newremain = remain-powv*mybit
            in
              (retlist@[mybit], newremain)
            end
        val (l, junk) = recFn 0
      in
        l
      end

      fun listToInt list =
        let
          fun recFn (c, (bit, ret)) = if bit = ~1 then (0, ret)
                                      else (bit-1, c*(Real.toInt IEEEReal.TO_NEAREST (Math.pow(Real.fromInt 2, Real.fromInt bit)))+ret)
          val (bit, ret) = List.foldl recFn ((fvectorbits-1),0) list 
        in
          ret
        end
*)
end
