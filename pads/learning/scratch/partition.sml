(*
Zach DeVito
The partitions of the dataset described by the TANE algorithm with associated
operations on it
*)

structure Partition = struct
open Common
exception Unimplemented
                (* e * number of eq classes * partition itself*)
type partition = int * int * (int list list)

(* allocated a table to hold data for union *)
val size = ref ( 0 )
val Table = ref(Array.array(0,0))

(* called once before each dataset to initialze the size variables
to the number of rows in the data*)
fun init(r : int):unit = 
( size := r;
  Table := Array.array(r+1,0);
  ()
)

structure BMap = RedBlackMapFn(struct
				type ord_key = Token option
				val compare = compare
				end)

(* returns a partition that represents the base data in a particular column *)				
fun fromData( bdolist : Token option list) : partition = 
let
	(* create the equivalence classes *)
	fun update(bdo,(n,bmap)) = 
	 (n+1, case BMap.find(bmap,bdo) of
	    SOME lst => BMap.insert(bmap,bdo,n :: lst) 
	  | NONE => BMap.insert(bmap,bdo,n :: nil) )
	 val (_,bmap) = foldr update (1,BMap.empty) bdolist
	 val part = BMap.listItems bmap
	 
	 (* filter out the single member classes to create stripped partitions
	 as described in TANE *)
	 val spart = List.filter (fn x => length x > 1) part
in
  toPart spart
end
(* create the empty partition (every value in one class) *)
and empty() = toPart [List.tabulate(!size, fn x => x + 1)]

and setT(i,v) = Array.update(!Table,i,v)
and getT(i) = Array.sub(!Table,i)
(* 
finds the union of two partitions
based on the algorithm given for TANE

because profiling revealed that this was a time critical
part of the code, the implementation has been taken almost 
directly from the paper.
*)
and union( (_,alen,alist), (_,blen,blist) ) : partition = 
let
	val S = Array.array(alen+1,(0,nil))
	val part = ref(nil)
	fun S_add( i , item) =
	let 
	  val (n,lst) = Array.sub(S,i)
	in
	  Array.update(S,i,(n+1,item::lst))
	end
	
	fun each_set(i,set) = map (fn t => setT(t,i)) set
	fun across_p1(h,i) = 
		(each_set(i,h); (i+1))
	val _ = foldr across_p1 1 alist
	fun each_set2(set) =
	let
	  fun each_item1(t) =
	  let 
	    val idx = getT(t)
	  in
	    if idx > 0 then S_add(idx,t) else ()
	  end
	  val _ = app each_item1 set
	  fun each_item2(t) =
	  let
	  	val idx = getT(t)
	  	val (n,lst) = Array.sub(S,idx)
	  	val _ = Array.update(S,idx,(0,nil))
	  in
	  	(*remove single member equivalence classes *)
	  	if n >= 2 then part := lst :: !part else ()
	  end
	  val _ = map each_item2 set
	in
	 ()
	end
	val _ = app each_set2 blist
	(* reset the table for future use *)
	val _ = app (app (fn t => setT(t,0))) alist 
in
  toPart(!part)
end
(* precomputes the error and size to create a partition from the actual 
partition data *)
and  toPart (list : int list list) = 
let 
  val ele = length list
  val sets = (foldr op+ 0 (map length list) )
in
 (ele - sets, ele, list)
end
(* is this partition a superkey? *)
fun isSKey( (e,_,_) : partition ) = e = 0
(* what is the error for this stripped partition *)
fun error( (e,_,_) : partition )  = e
end
