(*
Zach DeVito
Provides a Rational numbers type to 
get accurate dependencies for integers.
Uses LargeInts to prevent overflows at the cost of slower operations
*)

signature RATIONAL = sig 
type rat
val num : rat -> LargeInt.int
val denom : rat -> LargeInt.int
val + : (rat * rat) -> rat
val - : (rat * rat) -> rat
val * : (rat * rat) -> rat
val / : (rat * rat) -> rat
val equals : (rat * rat) -> bool
val fmt : StringCvt.radix -> rat -> string
val toString : rat -> string
val == : (rat * rat) -> bool
val < : (rat * rat) -> bool
val <= : (rat * rat) -> bool
val > : (rat * rat) -> bool
val >= : (rat * rat) -> bool
val fromInts : (int * int) -> rat
val ~ : rat -> rat
val sqrt : rat -> rat
val gcd : (LargeInt.int * LargeInt.int) -> LargeInt.int
val reduce : rat -> rat
val zero : rat
(*
val != : (rat * rat) -> bool *)
end

structure Rat : RATIONAL = struct
type rat = (LargeInt.int * LargeInt.int)

fun gcd (a,b) = if b = 0 then a else gcd(b, LargeInt.rem(a,b))
fun lcm (a,b) = let
				  val c = gcd(a,b)
				  val d = LargeInt.div(a,c)
				 in
				 	d * b
				 end
fun reduce (n,d) = let 
					val a = gcd(n,d) 
				    val (n',d') = (LargeInt.div(n,a), LargeInt.div(d,a))
				   in
				   	if d' < 0 then (~n', ~d') else (n',d')
				   end
fun num(a,b) : LargeInt.int = a
fun denom(a,b) : LargeInt.int = b

(* a/b + c/d = *)
fun doAdd(n1,d1,n2,d2) = 
let
	val m = lcm(d1,d2)
	val a1 = LargeInt.div(m,d1)
	val a2 = LargeInt.div(m,d2)
	in
		reduce(a1 * n1 + a2 * n2, m)
end

fun doMult(n1,d1,n2,d2) =
let
	val g1 = gcd(n1,d2)
	val g2 = gcd(n2,d1)
	val n1' = LargeInt.div(n1,g1)
	val n2' = LargeInt.div(n2,g2)
	val d1' = LargeInt.div(d1,g2)
	val d2' = LargeInt.div(d2,g1)
in
	reduce(n1' * n2', d1' * d2')
end

fun a + b = doAdd(num a, denom a, num b, denom b)
fun a * b = doMult(num a, denom a, num b, denom b)
fun a / b = doMult(num a, denom a, denom b, num b)
fun a - b = doAdd(num a, denom a, ~(num b), denom b)

exception notRootable
fun sqrt (n1,d1) = raise notRootable

fun equals(a,b) = ( (num a) = (num b) andalso (denom a) = (denom b) ) 
fun lt((n1,d1),(n2,d2)) = LargeInt.*(n1,d2) < LargeInt.*(n2,d1) 
fun gt((n1,d1),(n2,d2)) = LargeInt.*(n1,d2) > LargeInt.*(n2,d1) 
fun le((n1,d1),(n2,d2)) = LargeInt.*(n1,d2) <= LargeInt.*(n2,d1) 
fun ge((n1,d1),(n2,d2)) = LargeInt.*(n1,d2) >= LargeInt.*(n2,d1) 
fun not (n1,d1) = (LargeInt.~(n1),d1)
val == = equals
val op < = lt
val op <= = le
val op > = gt
val op >= = ge
val op ~ = not
fun fromInts(a,b) = reduce(Int.toLarge a,Int.toLarge b)
fun fmt rad (a,b) = case b of 
						  1 => (LargeInt.fmt rad a)
						| _ => "(" ^ (LargeInt.fmt rad a) ^ "/" ^ (LargeInt.fmt rad b) ^ ")"
fun toString r = fmt StringCvt.DEC r
val zero = fromInts(0,1)
end
