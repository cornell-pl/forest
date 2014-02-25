structure Partition = struct
type partition = int * int list
structure EQMap = RedBlackMapFn(struct
							type ord_key = int * int
							val compare = fn ((x1,y1),(x2,y2)) =>
											(case Int.compare(x1,x2) of
											   EQUAL => Int.compare(y1,y2)
											 | x => x)
							end)
structure EQHash = HashTableFn(struct
								 type hash_key = int * int
								 fun sameKey((a,b),(c,d)) = a = c andalso b = d
								 fun hashVal(a,b) = 
								 	let val h = 0w753 + (Word.fromInt a)
								 in
								 	0w33 * h + 0w720 + (Word.fromInt b)
								 end
								 
							   end)
exception HashNotFound			
fun size(a : partition) : int = #1 a
fun elements(a : partition) : int = length (#2 a)
fun union(a : partition, b: partition) : partition = 
	let
		fun combine(alist , blist, map,max) : int list = 
			case (alist,blist) of
			  (h1 :: t1, h2 :: t2) => (case (EQHash.find map)(h1,h2) of
			  							SOME x => x :: combine(t1,t2,map,max)
			  						  | NONE => max :: combine(t1,t2,((EQHash.insert map) ((h1,h2),max); map),max+1)
			  						  )
			 | (nil,nil)		   => max :: nil
		val c = combine(#2 a, #2 b,EQHash.mkTable(10000,HashNotFound),0)
	in
		(List.last c, List.take(c,(length c) - 1))
	end

fun pp2rint(h::t) = (print (Int.toString h); print ","; pp2rint (t))
  | pp2rint(nil) = print ")"
fun pprint(a,b) = (print ((Int.toString a) ^ " " ^  (Int.toString (elements((a,b)))) ^  "("); pp2rint b)
fun isSKey(a,b) = (a = length b)
end

structure FunctionalDep = struct
exception Unfound
exception Unfound2
structure AS = RedBlackSetFn(struct
			type ord_key = int
			val compare = Int.compare
			end)
			
fun ASintersect slist = foldr AS.intersection (hd slist) (tl slist)
fun ASunion		slist = foldr AS.union (hd slist) (tl slist)
structure ASMap = RedBlackMapFn(struct
			   type ord_key = AS.set
			   val compare = AS.compare
			   end)
			   
structure AS2 = RedBlackSetFn(struct
				type ord_key = AS.set
				val compare = AS.compare
				end)
				

fun find(m,i) = case ASMap.find(m,i) of SOME x => x | NONE => raise Unfound
fun find2(m,i) = case ASMap.find(m,i) of SOME x => x | NONE => AS.empty
fun compute_deps(level : AS2.set, candidates : AS.set ASMap.map, partition_map : Partition.partition ASMap.map, r: AS.set) : (AS.set * int) list * AS.set ASMap.map  =
	let
		fun each_x(x : AS.set, (deps : (AS.set * int) list, candidates': AS.set ASMap.map) ) = 
			let
				val newCand = ASintersect (map (fn y => find2(candidates', AS.delete(x,y))) (AS.listItems x))
				val candidates'' = ASMap.insert(candidates',x,newCand)
				
				fun isValid(aset, a) = (
											Partition.size( find(partition_map, aset) ) =
									   		Partition.size( find(partition_map, AS.add(aset, a)))
									   )
				fun iterate(a, (deplist, cand) ) = 
					let
					  val xminusa = AS.delete(x,a)
					  fun updateCand(cand') = 
					  	let
					  		val oldVal = find(cand',x)
					  		val oldValminusa = AS.delete(oldVal,a)
					  		val newVal = AS.difference(oldValminusa,AS.difference(r,x))
					  	in
					  		ASMap.insert(cand',x,newVal)
					  	end
					in
						if isValid(xminusa,a) then ( (xminusa,a) :: deplist, updateCand(cand) )
						else (deplist,cand)
					end
				val ret = AS.foldr iterate (deps,candidates'') (AS.intersection(x,newCand))
			in
				ret
			end
	in
		AS2.foldr each_x (nil,candidates) level
	end
fun printAS(a) = (print "(";app(fn y => print ((Int.toString y) ^ " ")) (AS.listItems a);print ")")
fun printAS2(as2) =  AS2.app (fn a => (print ""; printAS a; print "")) as2

fun prune(lev, candidates,partition_map,r) = 
let
	val levl = AS2.listItems lev
	val levl' = List.filter (fn x => not (AS.isEmpty (find(candidates,x)))) levl
	fun isSKey x =
	let
		val part = find(partition_map,x)
	in
		Partition.isSKey(part)
	end
	val (skeys,lev'') = List.partition isSKey levl'
	fun each_skey(x)  = 
	let
		fun each_a(a) =
		let
			val list = (map (fn z => find2(candidates, AS.delete(AS.add(x,a),z))) (AS.listItems x) )
			val list = ASintersect list
		in
			if AS.member(list,a) then SOME( (x, a) ) else NONE
		end
	in
		map each_a (AS.listItems (AS.difference(find(candidates,x),x)))
	end
	val depop = List.concat (map each_skey skeys)
	val deps = foldr (fn(dp, list) => case dp of SOME z => z :: list | NONE => list) nil depop
in
	(AS2.addList(AS2.empty,lev''), deps)
end


fun generate_next_level2(lastLevel : AS2.set, partition_map : Partition.partition ASMap.map) = 
let
	fun prefix_blocks(ll : AS2.set) = 
	let
		val sets = AS2.listItems(ll)
		val lists = map AS.listItems sets
		val listsets = ListPair.zip(lists,sets)
		fun is_prefix(small, big) = 
		 case (big,small) of
		   (h :: t, h2 :: t2) => if h = h2 then is_prefix(t2,t) else false
		 | (h :: t, nil) => true
		 | (nil, h :: t) => false
		 | (nil,nil) => true
		fun block_prefix nil = nil
		|   block_prefix x   = List.take(x, (length x) - 1)
		
		fun gen_blocks list = case list of
			(x,y) :: t => 
			let
				val (pos,neg) = List.partition (fn (nl,ns) => is_prefix(block_prefix x,nl) ) list
				val (_,sets) = ListPair.unzip(pos)
				val rest = gen_blocks(neg)
				
			in
				AS2.addList(AS2.empty,sets) :: rest
			end
				
		|   nil => nil
		val blocks = gen_blocks(listsets)
	in
		blocks
	end
	fun convolve(list) = case list of
		h :: t => ListPair.zip(List.tabulate(length t, fn _ => h), t) @ convolve(t)
	|   nil => nil
	val blocks = prefix_blocks(lastLevel)
	
	val pairs' = List.concat(map (fn x => convolve (AS2.listItems x)) blocks)
	val pairs = List.filter (fn(x,y) => not (AS.equal(x,y))) pairs'
	val newLev = List.filter (fn(x,y) => 
								let
									val X = AS.union(x,y)
									val b = AS.exists (fn z => not(AS2.member(lastLevel,(AS.delete(X,z)))) )  X
								in not(b) end
							) pairs
	val newPart = foldr ( fn((x,y),oldPart) => ASMap.insert(oldPart,AS.union(x,y),Partition.union(find(oldPart,x), find(oldPart,y)))) partition_map newLev
	val newLevSet = AS2.addList(AS2.empty, map AS.union newLev)
(*
	val _ = print "Start"
	val _ = map (fn x => (print "[";printAS2 x; print "]")) blocks
	val _ = print "\n"
	(*val _ = map (fn(x,y) =>(print "["; printAS(x); print ","; printAS(y); print "]")) newLev
	val _ = print "\n" *)
	val _ = (printAS2 lastLevel; print "\n")
	val _ = printAS2 newLevSet
	val _ = print "\n" *)
in
	(newLevSet,newPart)
end
fun print_part_map(part) = 
let
	val it = ASMap.listItemsi part
	(*val _ = map (fn (x,y) => (printAS(x); print " ==> "; Partition.pprint(y); print "\n")) it *)
in
	()
end
fun tane(partition_map : Partition.partition ASMap.map) = 
	let
		val (l1,_) = ListPair.unzip(ASMap.listItemsi partition_map)
		val l1set = AS2.addList(AS2.empty,l1)
		val l1set = AS2.delete(l1set, AS.empty)
		val R = ASunion( l1)
		val candidates = ASMap.insert(ASMap.empty,AS.empty,R)
		fun iterate(level, candidates, partition_map,size) = 
			let
				val _ = print ("Begin Level: " ^ (Int.toString size) ^ "\n")
				val (new_deps,new_cand) = compute_deps(level,candidates,partition_map,R)
				val (pl,maybe_deps) = prune(level,new_cand,partition_map,R)
				val (newLevel, new_part_map) = generate_next_level2(pl,partition_map)
				val keys = map #1 maybe_deps
			in
				if AS2.isEmpty newLevel then (print_part_map(new_part_map); (new_deps @ maybe_deps,keys))
				else let
						val (deps,keys') = iterate(newLevel,new_cand,new_part_map,size+1)
					 in
					 	(new_deps @ maybe_deps @ deps, keys @ keys')
					 end
			end
		fun depSetsToLists( deplist ) = case deplist of
		   (m,i) :: t => (AS.listItems(m),i) :: depSetsToLists(t)
		 | nil => nil
		val (deps,keys) = iterate(l1set,candidates,partition_map,1)
		val keyList = map AS.listItems keys
	in
		(depSetsToLists(deps), keyList)
	end


end
(*
  Int x => x
| Letters s => s
| Char c => c
| CharRep(c,i) => c
fun generate_next_level(lastLevel : AS2.set, partition_map : Partition.partition ASMap.map, size : int) = 
	let
		fun combine( a1, a2 , oldList, partition_map) = 
			let 
				val a3 = AS.union(a1,a2) 
				val s = AS.numItems(a3)
				fun isValid(a) = not( 
									  AS.exists ( fn x => not (AS2.member(lastLevel, AS.delete(a,x)))
									             ) a 
									 )
					
			in
				if s = size andalso isValid( a3 ) then 
					(AS2.add(oldList,a3), ASMap.insert(partition_map,a3, Partition.union( find(partition_map,a1), find(partition_map,a2) )))
				else
					(oldList, partition_map)
			end
		val ret = 
			 AS2.foldr (fn(a1, (nextLevel, partition_map)) =>
			  AS2.foldr (fn(a2, (nextLevel', partition_map')) =>	
			   			combine(a1,a2,nextLevel',partition_map')
			         ) (nextLevel, partition_map) lastLevel
			  ) (AS2.empty,partition_map) lastLevel
	in
		ret
	end
*)
(*
fun compute_deps2(level : AS2.set, candidates : AS.set ASMap.map, partition_map : Partition.partition ASMap.map)  =
let
	fun gen_cand_x(x) = (x, ASintersect (map (fn a => find(candidates, AS.delete(x,a))) (AS.listItems x)))
	val newCandsList = map gen_cand_x (AS2.listItems level)
	val newCands = foldr ASMap.insert' candidates newCandsList
	fun check_x(x) = 
	let
		val cand_x = find(newCands,x)
		val a_list = AS.intersection(x,cand_x)
		fun check_a(a) = 
		let
			fun isValid(xminusa,x) = Partition.size( find(partition_map,xminusa) )
									 = Partition.size( find(partition_map,x) )
			val xminusa = AS.delete(x,a)
		in
			if isValid(xminusa,x)
			then
				(* the dep     the set of stuff to remove from cand_x *)
				SOME(xminusa,a)
			else
				( NONE )
		end
		val depOptions = map check_a (AS.listItems x)
		val deps = foldr (fn(d, deps) => case d of SOME x => x :: deps | NONE => deps) nil depOptions
	in
		(deps)
	end
	val deps = map check_x (AS2.listItems level)
	val deps' = List.concat deps
	val newCands' = foldr (fn((x,a), c) => ASMap.insert(newCands,x,AS.delete(find(newCands,x),a))) newCands deps'
in
	(deps', newCands')
end *)