structure Partition = struct
type partition = int * int list
structure EQMap = SplayMapFn(struct
							type ord_key = int * int
							val compare = fn ((x1,y1),(x2,y2)) =>
											(case Int.compare(x1,x2) of
											   EQUAL => Int.compare(y1,y2)
											 | x => x)
							end)
fun union(a : partition, b: partition) : partition = 
	let
		fun combine(alist , blist, map,max) : int list = 
			case (alist,blist) of
			  (h1 :: t1, h2 :: t2) => (case EQMap.find(map,(h1,h2)) of
			  							SOME x => x :: combine(t1,t2,map,max)
			  						  | NONE => max :: combine(t1,t2,EQMap.insert(map,(h1,h2),max),max+1)
			  						  )
			 | (nil,nil)		   => max :: nil
		val c = combine(#2 a, #2 b,EQMap.empty,0)
	in
		(List.last c, List.take(c,(length c) - 1))
	end
fun size(a : partition) : int = #1 a
end

structure FunctionalDep = struct

structure AS = RedBlackSetFn(struct
			type ord_key = int
			val compare = Int.compare
			end)
structure ASMap = RedBlackMapFn(struct
			   type ord_key = AS.set
			   val compare = AS.compare
			   end)
structure AS2 = RedBlackSetFn(struct
				type ord_key = AS.set
				val compare = AS.compare
				end)
				
exception Unfound 
exception Unfound2
fun find(m,i) = case ASMap.find(m,i) of SOME x => x | NONE => raise Unfound
fun find2(m,i) = case ASMap.find(m,i) of SOME x => x | NONE => raise Unfound2
fun compute_deps(level : AS.set list, candidates : AS.set ASMap.map, partition_map : Partition.partition ASMap.map, r: AS.set) : (AS.set * int) list * AS.set ASMap.map  =
	let
		fun each_x(x : AS.set, (deps : (AS.set * int) list, candidates': AS.set ASMap.map) ) = 
			let
				fun intersect(a, newCandidate) = AS.intersection(newCandidate, find(candidates', AS.delete(x,a)))
				val newCand = AS.foldr intersect r x
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
		List.foldr each_x (nil,candidates) level
	end
fun generate_next_level(lastLevel : AS.set  AS2.set, partition_map : Partition.partition ASMap.map, size : int) = 
	let
		val lastLevelSet = AS2.addList(AS2.empty,lastLevel)
		fun combine( a1, a2 , oldList, partition_map) = 
			let 
				val a3 = AS.union(a1,a2) 
				val s = AS.numItems(a3)
				fun isValid(a) = not( 
									  AS.exists ( fn x => not (AS2.member(lastLevelSet, AS.delete(a,x)))
									             ) a 
									 )
					
			in
				if s = size andalso isValid( a3 ) then 
					(a3 :: oldList, ASMap.insert(partition_map,a3, Partition.union( find(partition_map,a1), find(partition_map,a2) )))
				else
					(oldList, partition_map)
			end
		val ret = 
			 foldr (fn(a1, (nextLevel, partition_map)) =>
			   foldr (fn(a2, (nextLevel', partition_map')) =>	
			   			combine(a1,a2,nextLevel',partition_map')
			         ) (nextLevel, partition_map) lastLevel
			  ) ([],partition_map) lastLevel
	in
		ret
	end
fun prune(level, candidates) = 
	let
		val level' = List.filter (fn x =>not (AS.isEmpty(find(candidates,x)))) level
	in
		level'
	end
fun tane(partition_map : Partition.partition ASMap.map) = 
	let
		val (l1,_) = ListPair.unzip(ASMap.listItemsi partition_map)
		val R = foldr (fn (key,set) => AS.union(set,key) ) AS.empty l1
		val candidates = ASMap.insert(ASMap.empty,AS.empty,R)
		fun iterate(level, candidates, partition_map,size) = 
			let
				val (new_deps,new_cand) = compute_deps(level,candidates,partition_map,R)
				val pl = prune(level,new_cand)
				val (newLevel, new_part_map) = generate_next_level(pl,partition_map,size+1)
			in
				case newLevel of
				 nil => new_deps
				| _  => new_deps @ iterate(newLevel,new_cand,new_part_map,size+1)
			end
	in
		iterate(l1,candidates,partition_map,1)
	end
fun printDeps( deplist ) = case deplist of
	(m,i) :: t => (AS.listItems(m),i) :: printDeps(t)
 | nil => nil
end
(*
  Int x => x
| Letters s => s
| Char c => c
| CharRep(c,i) => c

*)