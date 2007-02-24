(*
Zach DeVito
This structure implements the TANE algorithm using the Partition structure
*)
structure FunctionalDep = struct

(* Attribute Set *)
structure AS = RedBlackSetFn(struct
			type ord_key = int
			val compare = Int.compare
			end)
			
fun ASintersect slist = foldr AS.intersection (hd slist) (tl slist)
fun ASunion		slist = foldr AS.union (hd slist) (tl slist)

(* Map key'd on attribute sets *)
structure ASMap = RedBlackMapFn(struct
			   type ord_key = AS.set
			   val compare = AS.compare
			   end)
(* a set of attribute sets *)			   
structure AS2 = RedBlackSetFn(struct
				type ord_key = AS.set
				val compare = AS.compare
				end)
				
exception Unfound
exception Unfound2

(* find something that should be in the map *)
fun find(m,i) = case ASMap.find(m,i) of SOME x => x | NONE => raise Unfound
(* find something that may or may not be in the map*)
fun find2(m,i) = case ASMap.find(m,i) of SOME x => x | NONE => AS.empty

(* 
find the deps using the current level, the candidates, and the partition_map 
see the TANE paper for an overview of how the function accomplishes the task
*)
fun compute_deps(level : AS2.set, candidates : AS.set ASMap.map, partition_map : Partition.partition ASMap.map, r: AS.set) : (AS.set * int) list * AS.set ASMap.map  =
	let
		fun each_x(x : AS.set, (deps : (AS.set * int) list, candidates': AS.set ASMap.map) ) = 
			let
				val newCand = ASintersect (map (fn y => find2(candidates', AS.delete(x,y))) (AS.listItems x))
				val candidates'' = ASMap.insert(candidates',x,newCand)
				
				(* use the Partition.error function since we use stripped partitions *)
				fun isValid(aset, a) = (
											Partition.error( find(partition_map, aset) ) =
									   		Partition.error( find(partition_map, AS.add(aset, a)))
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

(* implements TANE's prune function, that removes/finds Keys and empty candidates *)
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

(* finds the next level in the lattice based on the previous level *)
fun generate_next_level(lastLevel : AS2.set, partition_map : Partition.partition ASMap.map) = 
let
    (* uses the fact that the red-black sets are alphabetical to find 
       prefix blocks described in the TANE paper *)
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
(* debugging to view the part_map *)
fun print_part_map(part) = 
let
	val it = ASMap.listItemsi part
	(*val _ = map (fn (x,y) => (printAS(x); print " ==> "; Partition.pprint(y); print "\n")) it *)
in
	()
end
(* main function of the tane algorithm using precomputed partitions *)
fun tane(partition_map : Partition.partition ASMap.map) = 
	let
		(* initialize the first level, and the candidates *)
		val (l1,_) = ListPair.unzip(ASMap.listItemsi partition_map)
		val l1set = AS2.addList(AS2.empty,l1)
		val l1set = AS2.delete(l1set, AS.empty)
		val R = ASunion( l1)
		val candidates = ASMap.insert(ASMap.empty,AS.empty,R)
		(* iterate through the levels *)
		fun iterate(level, candidates, partition_map,size) = 
			let
				val _ = if Options.print_levels then print ("Begin Level: " ^ (Int.toString size) ^ "\n") else ()
				val (new_deps,new_cand) = compute_deps(level,candidates,partition_map,R)
				val (pl,maybe_deps) = prune(level,new_cand,partition_map,R)
				val (newLevel, new_part_map) = generate_next_level(pl,partition_map)
				val new_part_map = ASMap.filteri ( fn(set,_) => AS.numItems set >= size) new_part_map
				val keys = map #1 maybe_deps
			in
				if AS2.isEmpty newLevel then 
					(print_part_map(new_part_map); (new_deps @ maybe_deps,keys))
				else let
						val (deps,keys') = iterate(newLevel,new_cand,new_part_map,size+1)
					 in
					 	(new_deps @ maybe_deps @ deps, keys @ keys')
					 end
			end
		(* remove the sets so we can return the deps an easy to use form *)
		fun depSetsToLists( deplist ) = case deplist of
		   (m,i) :: t => (AS.listItems(m),i) :: depSetsToLists(t)
		 | nil => nil
		val (deps,keys) = iterate(l1set,candidates,partition_map,1)
		val keyList = map AS.listItems keys
	in
		(depSetsToLists(deps), keyList) (* return both the deps (int list) * int and the keys (int lists) *)
	end


end
