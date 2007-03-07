structure Complexity = struct

    (* Determine if a positive integer is a power of two.
       This is a tail recursive implementation
     *)
    fun isPowerTwo ( n : int ) : int option =
        let fun isPowerTwo'(n:int,acc:int):int option =
                ( case n of
                    0 => NONE
                  | 1 => SOME acc
                  | n => if n mod 2 = 0
                         then isPowerTwo'(n div 2,acc + 1)
                         else NONE
                )
        in isPowerTwo'(n,0)
        end

    (* Same thing for large integers *)
    fun isPowerTwoL ( n : LargeInt.int ) : int option =
        let fun isPowerTwo'(n:LargeInt.int,acc:int) : int option =
                ( case n of
                    0 => NONE
                  | 1 => SOME acc
                  | n => if n mod 2 = 0
                         then isPowerTwo'(LargeInt.div (n,2),acc + 1)
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
    datatype Complexity = Bits    of int
                        | Choices of int
                        | Precise of real

    (* Value to use to get compilation started *)
    val junkComplexity : Complexity = Bits ( ~ 1 )
    val zeroComplexity : Complexity = Bits 0
    val unitComplexity : Complexity = Bits 1
    val impossible     : Complexity = Bits ( ~ 1 )

    (* Log base 2 of a positive integer *)
    fun log2 (n:int):real = Math.ln (Real.fromInt n) / Math.ln (Real.fromInt 2)
    fun log2L (n:LargeInt.int):real =
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
        let fun bitsAndChoice ( b : int, c : int ) : Complexity =
                ( case (isPowerTwo c) of
                       NONE   => Precise ( Real.fromInt b + log2 c )
                     | SOME d => Bits    ( b + d )
                )
        in ( case x1 of
                  Bits b1    => ( case x2 of
                                       Bits b2    => Bits (b1 + b2)
                                     | Choices c2 => bitsAndChoice ( b1, c2 )
                                     | Precise p2 => Precise (p2 + Real.fromInt b1)
                                )
                | Choices c1 => ( case x2 of
                                       Bits b2    => bitsAndChoice ( b2, c1 )
                                     | Choices c2 => Choices ( c1 + c2 )
                                     | Precise p2 => Precise ( log2 c1 + p2 )
                                )
                | Precise p1 => ( case x2 of
                                       Bits b2    => Precise ( p1 + Real.fromInt b2 )
                                     | Choices c2 => Precise ( p1 + log2 c2 )
                                     | Precise p2 => Precise ( p1 + p2 )
                                )
           )
        end

    (* Multiply a complexity by an integer constant *)
    fun multComp ( n : int ) ( c : Complexity ) : Complexity =
        ( case c of
               Bits b    => Bits    ( n * b )
             | Choices c => Choices ( n * c )
             | Precise p => Precise ( Real.fromInt n * p )
        )

    (* Probabilities should be in the interval [0, 1] *)
    type Probability = real

    (* Convert a probability to a complexity *)
    fun prob2Complexity ( p : Probability ) : Complexity = Precise (~ (log2r p))

    (* Convert an integer to a complexity *)
    fun int2Complexity ( n : int ) : Complexity =
    ( case isPowerTwo n of
           NONE   => Precise (log2 n)
         | SOME p => Bits p
    )

    fun int2ComplexityL ( n : LargeInt.int ) : Complexity =
    ( case isPowerTwoL n of
           NONE   => Precise (log2L n)
         | SOME p => Bits p
    )

    fun sumComplexities ( cs : Complexity list ) : Complexity =
        foldl ( fn (c1,c2) => combine c1 c2 ) zeroComplexity cs

    (* Complexity from the number of choices *)
    fun cardComp ( l : 'a list ) : Complexity =
        Choices (length l)

    (* Convert complexity to the precise method *)
    fun toPrecise ( c : Complexity ) : Complexity =
        ( case c of
               Bits n    => Precise (Real.fromInt n)
             | Choices l => Precise (log2 l)
             | Precise p => Precise p
        )

    (* Convert a complexity to a string *)
    fun showComp ( c : Complexity ) : string =
        ( case c of
               Bits n    => "Bits "    ^ (Int.toString n)
             | Choices l => "Choices " ^ (Int.toString l)
             | Precise p => "Precise " ^ (Real.toString p)
        )

end
