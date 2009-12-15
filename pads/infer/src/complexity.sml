structure Complexity = struct

    (* Determine if a positive integer is a power of two.
       This is a tail recursive implementation
     *)
    fun isPowerTwo ( n : int ) : LargeInt.int option =
        let fun isPowerTwo'( n : int, acc : int ) : LargeInt.int option =
                ( case n of
                    0 => NONE
                  | 1 => SOME (Int.toLarge acc)
                  | n => if n mod 2 = 0
                         then isPowerTwo'( n div 2, acc + 1 )
                         else NONE
                )
        in isPowerTwo'(n,0)
        end

    (* Same thing for large integers *)
    fun isPowerTwoL ( n : LargeInt.int ) : LargeInt.int option =
        let fun isPowerTwo'( n : LargeInt.int, acc : int ) : LargeInt.int option =
                ( case n of
                    0 => NONE
                  | 1 => SOME (Int.toLarge acc)
                  | n => if n mod 2 = 0
                         then isPowerTwo'( LargeInt.div (n,2), acc + 1 )
                         else NONE
                )
        in isPowerTwo'(n,0)
        end

    (* Complexity type
       Sometimes it is easy to measure complexity in bits, sometimes
       it is easier to measure complexity in the number of choices.
       For example, in an int type, the number of bits is a good
       measurement, but for a union type, the number of choices is a
       good measurement. When you convert a number of choices to bits,
       you get a floating point number (unless the number
       of choices is a power of two). Similarly, when you combine
       complexities of bit and choices, you get a floating point number.
     *)
    datatype Complexity = Bits    of LargeInt.int
                        | Choices of LargeInt.int
                        | Precise of real

    (* Value to use to get compilation started *)
    val zeroComp   : Complexity = Bits 0
    val unitComp   : Complexity = Bits 1
    val impossible : Complexity = Bits ( ~ 1 )

    (* Log base 2 of a positive integer *)
    fun log2 (n:int):real = Math.ln (Real.fromInt n) / Math.ln (Real.fromInt 2)
    fun log2L (n:LargeInt.int):real =
      if n = 0 then 0.0
      else
      	Math.ln (Real.fromLargeInt n) / Math.ln (Real.fromInt 2)

    (* Log base 2 of a positive real *)
    fun log2r (r:real):real = Math.ln r / Math.ln (Real.fromInt 2)

    (* Real to a real power: a ** x *)
    fun power ( a: real ) ( x : real ) : real = Math.exp ( x * Math.ln a)

    (* Real to a power of two *)
    fun power2 ( x : real ) : real = power 2.0 x

    (* Power of two, for positive integer exponents, only works for n <= 29 *)
    fun twopower ( n : int ) : int =
        let fun twopower' ( a : int ) ( n : int ) : int =
                ( case n of
                       0 => a
                     | n => twopower' (2 * a) (n - 1)
                )
        in twopower' 1 n
        end

    (* Combine two complexities *)
    fun combine (x1:Complexity) (x2:Complexity):Complexity =
        let fun bitsAndChoice ( b : LargeInt.int, c : LargeInt.int ) : Complexity =
                ( case (isPowerTwoL c) of
                       NONE   => Precise ( Real.fromLargeInt b + log2L c )
                     | SOME d => Bits    ( b + d )
                )
        in ( case x1 of
                  Bits b1    => ( case x2 of
                                       Bits b2    => Bits (b1 + b2)
                                     | Choices c2 => bitsAndChoice ( b1, c2 )
                                     | Precise p2 => Precise (p2 + Real.fromLargeInt b1)
                                )
                | Choices c1 => ( case x2 of
                                       Bits b2    => bitsAndChoice ( b2, c1 )
                                     | Choices c2 => Choices ( c1 + c2 )
                                     | Precise p2 => Precise ( log2L c1 + p2 )
                                )
                | Precise p1 => ( case x2 of
                                       Bits b2    => Precise ( p1 + Real.fromLargeInt b2 )
                                     | Choices c2 => Precise ( p1 + log2L c2 )
                                     | Precise p2 => Precise ( p1 + p2 )
                                )
           )
        end

    (* Multiply a complexity by an integer constant *)
    fun multCompS ( n : int ) ( c : Complexity ) : Complexity =
        ( case c of
               Bits b    => Bits    ( Int.toLarge n * b )
             | Choices c => Choices ( Int.toLarge n * c )
             | Precise p => Precise ( Real.fromInt n * p )
        )
    (* Same thing for large integers *)
    fun multComp ( n : LargeInt.int ) ( c : Complexity ) : Complexity =
        ( case c of
               Bits b    => Bits    ( n * b )
             | Choices c => Choices ( n * c )
             | Precise p => Precise ( Real.fromLargeInt n * p )
        )

    fun multCompR ( r : real ) ( c : Complexity ) : Complexity =
        ( case c of
               Bits b    => Precise ( r * Real.fromLargeInt b )
             | Choices c => Precise (log2r ( r * Real.fromLargeInt c ))
             | Precise p => Precise ( r * p )
        )
    fun divComp (n: LargeInt.int) (c: Complexity) : Complexity =
        ( case c of
               Bits b    => Precise ( (Real.fromLargeInt b) / 
				      (Real.fromLargeInt n) )
             | Choices c => Precise (log2r ( (Real.fromLargeInt c) /
				      (Real.fromLargeInt n) ))
             | Precise p => Precise ( p / (Real.fromLargeInt n) )
        )


    (* Probabilities should be in the interval [0, 1] *)
    type Probability = real

    (* Convert a probability to a complexity *)
    fun prob2Comp ( p : Probability ) : Complexity = Precise (~ (log2r p))

    (* Convert an integer to a complexity *)
    fun int2CompS ( n : int ) : Complexity =
    if n < 0
    then int2CompS ( ~n )
    else if n = 0
         then zeroComp
         else case isPowerTwo n of
                   NONE   => Precise (log2 n)
                 | SOME p => Bits p

    fun int2Comp ( n : LargeInt.int ) : Complexity =
    if n < 0
    then int2Comp ( ~n )
    else if n = 0
         then zeroComp
         else case isPowerTwoL n of
                   NONE   => Precise (log2L n)
                 | SOME p => Bits p

    (*function to convert the number of bits (real) needed to encode an integer*)
    fun int2Bits (n: LargeInt.int) : real =
	if n<0
	then 1.0 + int2Bits (~n)
	else log2L (n+1)

    fun sumComps ( cs : Complexity list ) : Complexity =
        foldl ( fn (c1,c2) => combine c1 c2 ) zeroComp cs

    fun realComp ( c : Complexity ) : real =
        ( case c of
               Bits n    => Real.fromLargeInt n
             | Choices l => log2L l
             | Precise p => p
        )

    fun maxComp ( c1 : Complexity ) ( c2 : Complexity ) : Complexity =
    if realComp c1 < realComp c2
    then c2
    else c1
        
    fun maxComps ( cs : Complexity list ) : Complexity =
        foldl ( fn (c1, c2) => maxComp c1 c2 ) zeroComp cs

    (* Complexity from the number of choices *)
    fun cardComp ( l : 'a list ) : Complexity =
(*
      if !(Config.var_card_bits) then       
        Choices (Int.toLarge (length l))
      else 
*)
	(Bits 32) (* max 2^32 branches in structs and unions *)


    (* Convert complexity to the precise method *)
    fun toPrecise ( c : Complexity ) : Complexity =
        ( case c of
               Bits n    => Precise (Real.fromLargeInt n)
             | Choices l => Precise (log2L l)
             | Precise p => Precise p
        )
    (* Convert compleixy to a real number *)
    fun toReal ( c : Complexity ) : real =
        ( case c of
               Bits n    => (Real.fromLargeInt n)
             | Choices l => log2L l
             | Precise p => p
        )

    (* Convert a complexity to a string *)
    fun showComp ( c : Complexity ) : string =
        ( case c of
               Bits n    => "Bits "    ^ (LargeInt.toString n)
             | Choices l => "Choices " ^ (LargeInt.toString l)
             | Precise p => "Precise " ^ (Real.toString p)
        )

    (* Show the complexity in bits *)
    fun showBits ( c : Complexity ) : string =
        ( case c of
               Bits n    => (LargeInt.toString n)  ^ "b"
             | Choices l => (Real.fmt (StringCvt.FIX (SOME 2)) (log2L l))  ^ "b"
             | Precise p => (Real.fmt (StringCvt.FIX (SOME 2)) p) ^ "b"
        )

    fun mDownFromN ( m : LargeInt.int ) ( n : LargeInt.int ) : LargeInt.int list =
        if n <= m
        then [m]
        else n :: mDownFromN m ( n - 1 )

    fun mUpToN ( m : LargeInt.int ) ( n : LargeInt.int ) : LargeInt.int list =
        rev ( mDownFromN m n )

    fun binomial ( n : LargeInt.int ) ( k : LargeInt.int ) : LargeInt.int =
    let val k' = LargeInt.min ( k, n - k )
    in if n <= k
       then 1
       else ( foldl ( fn (a,b) => a * b ) 1 ( mDownFromN ( n - k' + 1 ) n ) ) div
            ( foldl ( fn (a,b) => a * b ) 1 ( mDownFromN 1 k' ) )
    end

    fun average ( ns : int list ) : real =
    let fun sumCount ( n : int, sc : int * int ) : int * int = ( #1 sc + n, #2 sc + 1 )
        val ( sum, count ) = foldl sumCount (0,0) ns
    in if count = 0
       then 0.0
       else Real.fromInt sum / Real.fromInt count
    end

end
