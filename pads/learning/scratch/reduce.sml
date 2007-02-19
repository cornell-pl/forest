(* 
Zach DeVito
Kenny Zhu
Reduce implements the simplification system for Tys
*)
structure Reduce = struct
open Common
open Types
exception TyMismatch

(* calculates the complexity of a datatype so that we can try to minimize it*)
fun cost const_map ty =
  let 
	fun is_base ty' = case ty' of 
		Base _ => true 
		| RefinedBase _ => true
		| _ => false
	fun const_cost isbase c =
  		case c of
		    Length _ => if isbase then 1 else 0
		  | Ordered _ => if isbase then 1 else 0
		  | Unique _ => if isbase then 1 else 0
		  | Range _ => if isbase then 1 else 0
		  | Switched (_,ops) => 5
		  | Eq _ => 1
		  | EnumC set => if isbase then 5 else 0
	fun total_const_cost myty = 
	  let
		val id = getLabel(getAuxInfo(myty))
	  	val entry = LabelMap.find(const_map, id)
		val isbase = is_base(myty)
	  	val e_cost = case entry of 
	  	    SOME x => foldr op+ 0 (map (const_cost isbase) x)
	  	  | NONE => 0
	  in
	  	e_cost + 1 (* every constraint is counted towards cost *)
	  end
  in
	case ty of 
	  RefinedBase _ => 1 (* consts are cheaper than variables *)
	| Base _ => 2 
	| TBD _ => 1
	| Bottom _ => 1
	(* bigger datastructures are more complex than bases*)
	| Pstruct(a, tylist) => foldr op+ 0 (map (cost const_map) tylist) + 3 
	| Punion (a, tylist) => foldr op+ 0 (map (cost const_map) tylist) + 3
	| Parray (a, {tokens, lengths, first, body, last}) => 
			foldr op+ 0 (map (cost const_map) [first, body, last])+3
	| Switch (a, id, l) => foldr op+ 0 (map (fn (_, t) => cost const_map t) l)+3
	| RArray (a, sepop, termop, body, refinedop) =>
		(case sepop of SOME (sep) => cost const_map sep
			| NONE => 0) +
		(case termop of SOME (term) => cost const_map term 
			| NONE => 0) + (cost const_map body) +
		(case refinedop of SOME _ => 1
			| NONE => 0) 
  end

type constraint_map = constraint list LabelMap.map

(* a rule takes an Ty and the constrant lookup table, returns a [possibly] new Ty *)

(* 	they are seperated into two types: pre and post constraint anaysis
	this allows for a reduction in complexity without worrying about labels
	changes due to major structural changes
*)
type pre_reduction_rule = Ty -> Ty
type post_reduction_rule = constraint_map -> Ty -> constraint_map*Ty

(* reduction rules *)
(* single member lists are removed*)
fun remove_degenerate_list ty =
case ty of
  Punion(a, h :: nil) => h
| Punion (a, nil) => Base(a, nil)
| Pstruct (a, h :: nil) => h
| Pstruct (a, nil) => Base(a, nil)
| Switch(a, id, nil) => Base(a, nil)
| Parray(a, {tokens, lengths, first, body, last}) => 
	if lengths=nil then Base(a, nil) else ty 
| RArray(a, _, _, _, length_op) => 
	(
	case some(length_op) of
		IntConst(0) => Base(a, nil) 
	| _ => ty
	)
| _ => ty
(* tuples inside tuples are removed*)
and unnest_tuples ty : Ty =
case ty of 
  Pstruct (a, tylist) =>
  let
  	fun lift_tuple ty = 
  	case ty of
  	  Pstruct(_, tylist) => tylist
  	| _ => [ty]
  	val result = map lift_tuple tylist
  in
  	Pstruct(a, List.concat result )
  end
| _ => ty
(* sums inside sums are removed *)
and unnest_sums ty : Ty = 
case ty of 
  Punion(a, tylist) =>
  let
  	fun lift_sum ty = 
  	case ty of
  	  Punion(_, tylist) => tylist
  	| _ => [ty]
  	val result = map lift_sum tylist
  in
  	Punion(a, List.concat result )
  end
| _ => ty
(* remove nil items from struct*)
and remove_nils ty : Ty = 
case ty of
  Pstruct (a, tylist) => 
    Pstruct(a, List.filter (fn x => case x of 
			Base(_, nil) => false 
			| RefinedBase(_, _, nil) => false
			| _ => true ) tylist)
| _ => ty
(* elements of a sum are check to see if they share a common prefix (ie tuples with
a common prefix, or a common postfix) these elements are then brought out of a sum
and a tuple is created *)
(* the token coverage in the aux info may not be correct after this operation *)
and prefix_postfix_sums ty : Ty =
case ty of
  Punion (a, tylist) =>
  let
  	fun conv_to_list ty' =
  	case ty' of
  	  Pstruct(_, tylist') => tylist'
  	| _ => [ty']
  	val underlings = map conv_to_list tylist (*list of tylists *)
	val auxlist = map getAuxInfo tylist (* list of aux info *)
  	fun commonPrefix tylists = 
  	let
  		val elems = map hd tylists
  		val tails = map tl tylists
  	in
		case elems of
		  h :: t => 
		  let 
		  	val not_equal = List.exists (fn x => not(ty_equal(x,h) )) t
		  in 
		  	if not_equal then nil else h :: commonPrefix tails
  	          end
		| nil => nil
  	end handle Empty => nil
  	val cpfx = commonPrefix underlings
  	val plen = length cpfx
  	val remaining = map (fn x => List.drop(x,plen) ) underlings
  	val remaining_rev = map List.rev remaining
  	val csfx_rev = commonPrefix remaining_rev
  	val slen = length csfx_rev
  	val remaining_rev = map (fn x => List.drop(x,slen) ) remaining_rev
  	val remaining = map List.rev remaining_rev
  	val csfx = List.rev csfx_rev
  	val rem_tups = map (fn x => Pstruct(x) ) (ListPair.zip(auxlist, remaining))
  	val rem_reduced = map (remove_degenerate_list) rem_tups
  in
  	case (cpfx, csfx) of
  	  (h::t, _) => Pstruct ({coverage=(#coverage a), 
			label=SOME(getLabel({coverage=0, label=NONE}))}, 
				cpfx @ [ Punion(a, rem_reduced) ] @ csfx)
  	| (_,h::t) => Pstruct ({coverage=(#coverage a), 
			label=SOME(getLabel({coverage=0, label=NONE}))}, 
				cpfx @ [ Punion(a, rem_reduced) ] @ csfx)
  	| (nil,nil) => Punion (a, rem_reduced)
  end
| _ => ty
(* adjacent constant strings are merged together *)
and adjacent_consts ty : Ty = 
  case ty of Pstruct(a, tylist) => 
    let
	 fun mergetok (t1:LToken, t2:LToken) : LToken =
	    let
		fun combineloc (loc1:location, loc2:location) = 
		  {lineNo=(#lineNo loc1), beginloc=(#beginloc loc1), endloc=(#endloc loc2)}
	    in
		case (t1, t2) of 
			((Pstring(s1), loc1), (Pstring(s2), loc2)) => 
			  (Pstring(s1 ^ s2), combineloc(loc1, loc2))
			| ((Pwhite(s1), loc1), (Pwhite(s2), loc2)) => 
			  (Pwhite(s1 ^ s2), combineloc(loc1, loc2))
			| ((Other(a), loc1), (Other(b), loc2)) => 
			  (Pstring(Char.toString(a) ^ Char.toString(b)), 
					combineloc(loc1, loc2))
			| ((Pempty, loc1), (Pempty, loc2)) => (Pempty, combineloc(loc1, loc2))
			| _ => raise TyMismatch
	    end
	 (*the two token lists are supposed to be of equal length*)
	 fun mergetoklist (tl1: LToken list, tl2: LToken list): LToken list =
		if (length tl1 <> length tl2) then raise TyMismatch
		else case tl2 of 
			nil => tl1
			| _ => ListPair.map mergetok (tl1, tl2)

  	 fun for_const while_const t x tl = 
  	 let
       		val (clist,rest, resttl) = while_const(t)
     	 in
	  	(x :: clist, rest, mergetoklist(tl, resttl))
         end
  	 fun while_const tylist = case tylist of 
  		h::t => (case h of
  				  RefinedBase(_, StringConst(x), tl) => 
					for_const while_const t x tl
  				| _ => (nil,tylist, nil))
  	   	| nil => (nil, nil, nil)

  	 fun find_adj tylist = case tylist of
  	  	h::t => (case h of
			RefinedBase(aux, StringConst(x), l) => 
			let
  				val (clist, rest, tlists) = while_const(t)
    			in
    				RefinedBase(aux, StringConst(String.concat(x :: clist)), 
				 mergetoklist(l, tlists)) 
				:: find_adj rest
    			end
  	    			| _ => h :: find_adj t)
  		| nil => nil
  	val newtylist = find_adj tylist
    in
  	Pstruct(a, newtylist)
    end
| _ => ty
(* removed unused branches of sums *)
and unused_branches ty =
case ty of 
  Punion(aux, tylist) => 
  let
  	fun isUnused(ty) = ( 
  	  case ty of
	    Base(_, nil) => true
	    | RefinedBase(_, _, nil) => true
  	    | _ => false)
  	fun remove_unused() = 
  	let
  		val (unused,used) = List.partition isUnused tylist
  		val strs = map TyToString unused
  		(*val _ = app (fn x=> print ("unused:" ^ x)) strs*)
  	in
  		used
  	end
  in
  	Punion (aux, remove_unused())
  end
| _ => ty
		
(* post constraint rules, these require the cmap to be filled  and the 
data labeled *)

(* a unique Base type becomes a constant type *)
(* Notice that the unique constraint has not been taken away from the LabelMap *)
and uniqueness_to_const cmos ty =
case ty of
  Base({coverage, label=SOME id}, tokens) => 
    (case LabelMap.find(cmos, id) of  
      SOME consts => 
        let
      		fun find_unique clist newlist = case clist of
      	    		Unique x :: t => (SOME(x), newlist @ t)
      	  		| h :: t => find_unique t (newlist@[h])
      	  		| nil => (NONE, newlist)
		val (somety, newconsts) = find_unique consts nil
		val (newcmos, _) = LabelMap.remove(cmos, id)
	    	val newcmos = LabelMap.insert(newcmos, id, newconsts)
        in
       		case somety of
         	  SOME(Pint(x)) => 
			(
				newcmos, 
				RefinedBase({coverage=coverage, label = SOME id}, 
				IntConst(x), tokens)
			)
		| SOME(Pstring(x)) => 
			(
				newcmos, 
				RefinedBase({coverage=coverage, label = SOME id}, 
				StringConst(x), tokens)
			)
		| SOME(Ptime(x)) => 
			(
				newcmos, 
				RefinedBase({coverage=coverage, label = SOME id}, 
				StringConst(x), tokens)
			)
		| SOME(Pmonth(x)) => 
			(
				newcmos, 
				RefinedBase({coverage=coverage, label = SOME id},
				StringConst(x), tokens)
			)
		| SOME(Pip(x)) => 
			(
				newcmos, 
				RefinedBase({coverage=coverage, label = SOME id}, 
				StringConst(x), tokens)
			)
		| SOME(Pwhite(x)) => 
			(
				newcmos, 
				RefinedBase({coverage=coverage, label = SOME id},
				StringConst(x), tokens)
			)
       		| _ => (cmos, ty)
      	end
    | NONE => (cmos, ty)
    )
| _ => (cmos, ty)

(* convert a sum into a switched type if a switch constraint is defined *)
(* a sum can only be determined by a base value that is the son of any of the sum's
   ancestors, for a start, we just look at the sum's siblings in a tuple *) 
and sum_to_switch cmos ty =
case ty of 
  Pstruct(aux, tylist) =>
  let
	(* test if a sum is a switched sum depending on some other id*)
	fun is_switch(cmos, id) = 
	  case LabelMap.find(cmos, id) of  
      		SOME consts => 
        	  let 
			fun find_switch clist newclist =
      	  			case clist of 
				  Switched (ids, mappings) :: t => 
				    (* we are only interested in 1-1 mapping in switched *)
				    if length(ids) = 1 andalso 
					length(#1 (hd mappings))=1 
				    then (SOME (ids, mappings), newclist@t)
				    else find_switch t (newclist@[Switched(ids, mappings)])
      	  			  | h :: t => find_switch t (newclist@[h])
      	  			  | nil => (NONE, newclist)
			val (someidmappings, newconsts) = find_switch consts nil
			val (newcmos, _) = LabelMap.remove(cmos, id)
	    		val newcmos = LabelMap.insert(newcmos, id, newconsts)
		  in
			(someidmappings, newcmos)
		  end 
    	 	| NONE => (NONE, cmos) 
	(* function to test if a base value with a specific id exists in a ty list*) 
	fun existsbase(tylist, id) = 
		case tylist of Base({coverage, label=SOME id}, _)::tail => true
			| RefinedBase({coverage, label=SOME id}, _, _)::tail => true
			| _::tail => existsbase(tail, id)
			| nil => false
				
	fun to_switch (ty, id, mappings)=
	(* convert a union ty to a Switch type if possible given an id 
	   from a switch variable and mappings of a list of tuples 
	  ([token option], token option) *)
	  case ty of
	    Punion(aux, tlist) =>
	  	let
		    fun getrefine (index, mappings) = 
		    (*given an index, give the refined value from the mapping*)
		      case mappings of 
			([SOME tok1], SOME(Pint i))::tail => 
				if (i=index) then SOME(tokentorefine(tok1))
				else getrefine (index, tail)
		      	| _::tail => getrefine(index, tail)
		      	| nil => NONE
		    (*assume index starts from 1 for the tylist branches*)
		    fun   gen_ref_ty_list (_, nil, _) = nil
			| gen_ref_ty_list (mappings, hd::tail, index) =
			(case getrefine(index, mappings) of
			  SOME refined => (refined, hd):: 
				gen_ref_ty_list(mappings, tail, index+1)
			  | NONE => nil (* returns immediately if no refine is found *)
			)
		    val refine_ty_list = gen_ref_ty_list (mappings, tlist, 1)
		in
		    if (length refine_ty_list = length tlist) 
		    then Switch (aux, id, refine_ty_list)
		    else ty
		end
	   | _ => ty
		    
	fun rewrite_switch (cmos, tylist) =
	    case tylist of 
		h::rest => 
		(
		case h of 
			Punion(a, sumlist) => 
			  let 
			    val (c, newcmos)  = is_switch(cmos, some(#label a))
			  in 
			    case c of 
				SOME ([id], mappings) =>
					if existsbase(tylist, id)
			    		then 
					  let val (newcmos, rest') = rewrite_switch(newcmos, rest)
					  in
						(newcmos, to_switch(h, id, mappings)::rest')
					  end
			    		else 
					  let val (newcmos, rest') = rewrite_switch(newcmos, rest)
					  in
					  	(newcmos, h::rest')
					  end
				| _ =>    let val (newcmos, rest') = rewrite_switch(newcmos, rest)
					  in
					  	(newcmos, h::rest')
					  end
			  end
			| _ => 	let val (newcmos, rest')=rewrite_switch(cmos, rest)
		       		in (newcmos, h::rest')
				end
		)
		| nil => (cmos, nil)

	val (newcmos, tylist') = rewrite_switch(cmos, tylist)
  in 
	(newcmos, Pstruct(aux, tylist'))
  end
  | _ => (cmos, ty)

(*convert an enum constraint to a Enum refined type or a range constraint to 
	a range refined type *)
(*Notice constraints are not deleted from the cmap yet*)
and enum_range_to_refine cmos ty = 
  case ty of                  
    Base({coverage, label=SOME(id)}, b) =>         
    (case LabelMap.find(cmos,id) of 
      SOME consts =>    
        let             
                fun check_enum list newconsts=  
                  case list of (EnumC set) :: t =>
                      let
                        val items = BDSet.listItems set
                        val refs = map tokentorefine items
                        val ty' = RefinedBase({coverage=coverage, label = SOME id}, 
						Enum refs, b)
                     	(* val _ = print ("ENUM" ^ (tytos ty')) *)
                      in
                        (ty', newconsts@t)
                      end
		  | (Range(min,max)):: t => (RefinedBase({coverage=coverage, label = SOME id}, 
				Int(min, max), b), newconsts@t)
                  | h :: t => check_enum t (newconsts@[h])
                  | nil => (ty, newconsts) 
		val (newty, newconsts) = check_enum consts nil
		val (newcmos, _) = LabelMap.remove(cmos, id)
	    	val newcmos = LabelMap.insert(newcmos, id, newconsts)
        in
		(newcmos, newty)
        end
    | NONE => (cmos, ty)
    )
  | _ => (cmos, ty)

(* the actual reduce function can either take a SOME(const_map) or
NONE.  It will use the constraints that it can apply. *)
and reduce const_info_op ty = 
let
  val pre_constraint_rules : pre_reduction_rule list = 
		[ 	remove_degenerate_list,
			unnest_tuples,
			unnest_sums,
			prefix_postfix_sums,
			adjacent_consts,
			remove_nils,
		  	unused_branches
		]
  val post_constraint_rules : post_reduction_rule list =
		[ uniqueness_to_const,
		  enum_range_to_refine,
		  sum_to_switch
		  (* need to add a constraint for RArray *)
		]
  (* generate the lust of rules *)
  val cmap = case const_info_op of 
      SOME (cmap) => cmap
    | NONE => LabelMap.empty
  (* returns a new cmap after reducing all the tys in the list and a new tylist *)
  fun mymap f cmap tylist newlist =
	case tylist of 
		ty::tail => let 
				val(cmap', ty') = f cmap ty
			    in	mymap f cmap' tail (newlist@[ty'])
			    end
		| nil => (cmap, newlist)

  (*reduce a ty and returns the new constraint map and the new ty *)
  fun reduce' cmap ty =
    let 
      	(* go bottom up, calling reduce' on children values first *)
      	val (newcmap, reduced_ty) = 
			case ty of
			  Pstruct (a, tylist) => let 
				val (cmap', tylist') = mymap reduce' cmap tylist nil
				in (cmap', Pstruct (a, tylist'))
				end
			| Punion (a, tylist) => let
				val (cmap', tylist') = mymap reduce' cmap tylist nil
				in (cmap', Punion(a, tylist'))
				end
			| Parray (a, {tokens, lengths, first, body, last}) => 
				let
				val (cmap1, firstty) = reduce' cmap first
				val (cmap2, bodyty) = reduce' cmap1 body 
				val (cmap3, lastty) = reduce' cmap2 last 
				in
				(cmap2, Parray(a, {tokens=tokens, lengths=lengths, 
				first=firstty,
				body= bodyty,
				last=lastty}))
				end
			| Base b => (cmap, Base b)
			| TBD b => (cmap, TBD b)
			| Bottom b => (cmap, Bottom b)
			| RefinedBase b => (cmap, RefinedBase b)
			| Switch b => (cmap, Switch b)
			| RArray b => (cmap, RArray b)

	  fun iterate cmap ty = 
	  let
	    (* calculate the current cost *)
	    val cur_cost = cost cmap ty
	    (* apply each rule to the ty *)
	    val post_pairs = map (fn x => x cmap ty) post_constraint_rules
	    val pre_pairs = map (fn x =>(cmap, x ty)) pre_constraint_rules
	    val cmap_ty_pairs = post_pairs@pre_pairs
	    (* find the costs for each one *)
	    val costs = map (fn (m, t)=> cost m t) cmap_ty_pairs
	    val pairs = ListPair.zip(cmap_ty_pairs,costs)
	    fun min((a,b),(c,d)) = if b < d then (a,b) else (c,d)
	    (* find the minimum cost out of the ones found *)
	    val ((newcmap, newTy), lowCost) = foldr min ((cmap, ty), cur_cost) pairs
	  in
	  	(* as long as the cost keeps going down, keep iterating *)
	  	if lowCost < cur_cost then iterate newcmap newTy
	  	else (newcmap, newTy)
	  end
    in
 	(iterate newcmap reduced_ty)
    end
  val cbefore = cost cmap ty
  val (cmap', ty') = reduce' cmap ty
  val cafter = cost cmap' ty'
(*  val _ = print ("Before:" ^ (Int.toString cbefore) ^ " After:" ^ (Int.toString cafter) ^ "\n") *)
in
  ty'
end

(* now some tests *)
(******************
fun dotest() = 
let
	val a = Base IntBase
	val b = Base LettersBase
	val c = Base (ConstBase "c")
	val d = Base (REBase "*")
	val pre = Tuple [ a, b, d ]
	val post = Tuple [d, b, a ]
	
    val cmap_info = [ ("lab1", [Unique (Letters "a_unique_value")]), ("lab2",[]),("lab3",[Unique (Letters "a_unique_value")]), ("lab4",[Range (0,1)]) ]
	val labels = ["lab1","lab3","lab4"]
	
	fun wrap lab = SOME(Const lab)
	val cmap_info = [ 
					  ("s1", [ Range (1,3),
					           Switched (["s3"],
					            [
					              ([wrap "r101"],wrap "r1"),
					              ([wrap "r102"],wrap "r1"),
					              ([wrap "r103"],wrap "r1"),
					              ([wrap "r104"],wrap "r2"),
					              ([wrap "r105"],wrap "r2"),
					              ([wrap "r106"],wrap "r2")
					            ]),
					            Switched (["s2"],
					            [
					              ([wrap "r10"],wrap "r1"),
					              ([wrap "r11"],wrap "r1"),
					              ([wrap "r12"],wrap "r2"),
					              ([wrap "r13"],wrap "r2")
					            ])
					          ] ),
					   ("s2", [ Range (5,6),
					           Switched (["s3"],
					            [
					              ([wrap "r101"],wrap "r10"),
					              ([wrap "r102"],wrap "r11"),
					              ([wrap "r103"],wrap "r11"),
					              ([wrap "r104"],wrap "r12"),
					              ([wrap "r105"],wrap "r13"),
					              ([wrap "r106"],wrap "r13")
					            ])
					          ] ) 
					   
					] @ cmap_info
					            
	val labels = ["r1","r2","r10","r11","r12","r13","r101","r102","r103","r104","r105","r106"] @ labels
	
	val cmap = foldr (fn (a,m) => LabelMap.insert'(a,m) ) LabelMap.empty cmap_info
	val labset = LabelSet.addList(LabelSet.empty,labels)
	
	val tests = [ Tuple [], Sum [], Tuple [ Base LettersBase ], Sum [ Base LettersBase ],
				  Tuple[ Sum [ Sum [] ] ],
				  Tuple[ b,Tuple[a,a,a,a],b,Tuple[b,b,a]],
				  Sum[ Sum[a,b],Sum[c,d],a],
				  Sum [ Tuple[ pre,a,post], Tuple[pre,b,post], Tuple[pre,c,post] ],
				  Sum [ Tuple[ pre,a,post], Tuple[pre,b], Tuple[pre,c,post] ],
				  Sum [ Tuple[ pre,a,post], Tuple[pre,b,post], Tuple[c,post] ],
				  Tuple[ c,c,c ],
				  Tuple[ b,c,c,b,c,c,b,c,c,c],
				  Label("lab1",b),
				  Label("lab2",b),
				  Sum[a,Label("lab2", b),c,d]
				 ]
				 
	val r1 = Label("r1", Base (ConstBase "1"))
	val r2 = Label("r2", Base (ConstBase "2"))
	val r10 = Label("r10", Base (ConstBase "10"))
	val r11 = Label("r11", Base (ConstBase "11"))
	val r12 = Label("r12", Base (ConstBase "12"))
	val r13 = Label("r13", Base (ConstBase "13"))
	val r101 = Label("r101", Base (ConstBase "101"))
	val r102 = Label("r102", Base (ConstBase "102"))
	val r103 = Label("r103", Base (ConstBase "103"))
	val r104 = Label("r104", Base (ConstBase "104"))
	val r105 = Label("r105", Base (ConstBase "105"))
	val r106 = Label("r106", Base (ConstBase "106"))
	
	val test = Tuple [ a,Label("s1",Sum [ r1, r2]),Label("s2",Sum[ r10,r11,r12,r13 ]), Label("s3",Sum[ r101,r102,r103,r104,r105,r106 ]),a]
	
(*	val _ = print (irtos test)
	val (newIr,cmap) = sum_to_switch cmap labels test
	val _ = print (irtos newIr) *)
	val tests = tests @ [test]
	val pre_results = ( map (reduce (NONE)) tests )
	val results = ( map (reduce (SOME(cmap,labset))) pre_results )
	val strings = ListPair.zip(map irtos tests, map irtos results)
	val strings' = map (fn (x,y) => "From:\n" ^ x ^ "To:\n" ^ y ^ "\n\n\n") strings
	val _ = map print strings'
in
 strings
end
*************)

end
