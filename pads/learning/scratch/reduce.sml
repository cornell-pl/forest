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
		  | EnumC set => if isbase then 1 else 0
	fun total_const_cost myty = 
	  let
		val id = getLabel(getAuxInfo(myty))
	  	val entry = LabelMap.find(const_map, id)
		val isbase = is_base(myty)
	  	val e_cost = case entry of 
	  	    SOME x => foldr op+ 0 (map (const_cost isbase) x)
	  	  | NONE => 0
	  in
	  	e_cost 
	  end
	fun ty_cost myty =
		case myty of 
		(* consts are cheaper than variables *)
		  RefinedBase (_, r, _) =>  (
			case r of 
				StringME _ => 1 
				| Int _ => 2
				| IntConst _ => 1
				| StringConst _ => 1
				| Enum l => length(l)
				| LabelRef _ => 1
			)
		| Base _ => 3 
		| TBD _ => 1
		| Bottom _ => 1
		(* bigger datastructures are more complex than bases*)
		| Pstruct(a, tylist) => foldr op+ 0 (map (cost const_map) tylist) + 3 
		| Punion (a, tylist) => foldr op+ 0 (map (cost const_map) tylist) + 3
		| Parray (a, {tokens, lengths, first, body, last}) => 
			(* first, body and last are potentially structs so plus 9*)
				foldr op+ 0 (map (cost const_map) [first, body, last])+9
		| Switch (a, id, l) => foldr op+ 0 (map (fn (r, t) => 
			ty_cost (RefinedBase(a, r, nil))+cost const_map t) l)
		| RArray (a, sepop, termop, body, lenop, lens) => 
			(case sepop of SOME (sep) => 0 | NONE => 1) +
			(case termop of SOME (term) => 0 | NONE => 1) + 
			(cost const_map body) 
	in (ty_cost ty) + (total_const_cost ty) + 1 (* every constraint is counted towards cost *)
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
| RArray(a, _, _, _, _, nil) => Base(a, nil) 
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
(* also removes Pemptys from struct*)
and remove_nils ty : Ty = 
case ty of
  Pstruct (a, tylist) => 
    Pstruct(a, List.filter (fn x => case x of 
			Base(_, nil) => false 
			| Base (_, tl) => not(ltoken_equal((hd tl), 
					(Pempty, {lineNo=0, beginloc=0, endloc=0}))) 
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
		  	val not_equal = (List.exists (fn x => not(ty_equal(1, x, h) )) t) 
		  in 
		  	if not_equal then nil else (foldr mergeTy h t) :: commonPrefix tails 
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
  	val rem_tups = map(fn x => Pstruct(x)) (ListPair.zip(auxlist, remaining))
  	val rem_reduced = List.filter (fn x => case x of Pstruct(a, nil) => false 
						| _ => true) rem_tups
  in
  	case (cpfx, csfx) of
  	  (h::t, _) => Pstruct (mkTyAux (#coverage a), 
				cpfx @ [ Punion(a, rem_reduced) ] @ csfx)
  	| (_,h::t) => Pstruct (mkTyAux (#coverage a), 
				cpfx @ [ Punion(a, rem_reduced) ] @ csfx)
  	| (nil,nil) => Punion (a, rem_reduced)
  end
| _ => ty
(* detect a table with a header and rewrite the struct with unions inside
(a1 + b1), (a2 + b2), (a3 + b3) = (a1, a2, a3) + (b1, b2, b3) 
where a and b are header and body rows of a table respectively *)
(*this rule cause the cost of the ty to go up so it's currently not used *)
and extract_table_header ty =
 case ty of 
	Pstruct (a, tylist) =>
	  let
		fun getNewLabel x = SOME(getLabel ({coverage=x, label=NONE, 
					typeComp=Bits(0), dataComp=Bits(0)}))
		fun numUnions tylist =
			case tylist of
				h::tail => (case h of 
						Punion(_, _) => 1+numUnions tail
						| _ => numUnions tail
					   )
				| nil => 0
		fun check_table tylist =
		  case tylist of
			h::tail => 
				(
				case h of 
				Punion(a, [ty1, ty2]) =>
					let
						val c1 = getCoverage(ty1)
						val c2 = getCoverage(ty2)
					in
						if (c1=1 andalso c2 >1) orelse
						   (c1>1 andalso c2=1) then
							check_table tail
						else false	
					end
				| _ => check_table tail
				)
			| nil => true	 
		fun split_union ty =
			case ty of
				Punion(a, [ty1, ty2]) => 
					let
						val c1 = getCoverage(ty1)
						val c2 = getCoverage(ty2)
					in
						if (c1=1) then (ty1, ty2) (* table header comes first *)
						else (ty2, ty1)
					end
				| ty => let
					val aux = getAuxInfo(ty)
					val c= #coverage aux
					val l= #label aux
					val t= #typeComp aux
					val d= #dataComp aux
					val aux1 = {coverage=1, label=getNewLabel 1, typeComp=t, dataComp=d}
					val aux2 = {coverage=c-1, label=l, typeComp=t, dataComp=d}
					in
						(setAuxInfo ty aux1, setAuxInfo ty aux2)
					end
		val overallCoverage = getCoverage(ty)
		val unions = numUnions tylist
	  in
		if unions>=2 andalso check_table tylist = true then
		  let
			val _ = print "Found a table!!! Rewriting!!!\n"
			val _ = printTy ty
			val (tys1, tys2) = ListPair.unzip (map split_union tylist)
			val a1 = {coverage=1, label=getNewLabel 1, typeComp=Bits 0, dataComp=Bits 0}
			val a2 = {coverage=overallCoverage-1, 
				label=getNewLabel 1, typeComp=Bits 0, dataComp=Bits 0}
			val newty = Punion(a, [Pstruct(a1, tys1), Pstruct(a2, tys2)])
			val _ = (print "Cost for ty: "; print (Int.toString(cost LabelMap.empty ty)))
			val _ = (print "\nCost for newty: "; print (Int.toString(cost LabelMap.empty newty)))
		  in newty
		  end
		else ty
	  end
     	| _ => ty

(* adjacent constant strings are merged together *)
and adjacent_consts cmos ty = 
  case ty of Pstruct(a, tylist) => 
    let
	 fun mergetok (t1:LToken, t2:LToken) : LToken =
	    let
		fun combineloc (loc1:location, loc2:location) = 
		  {lineNo=(#lineNo loc1), beginloc=(#beginloc loc1), endloc=(#endloc loc2)}
	    in
		case (t1, t2) of 
			((Pwhite(s1), loc1), (Pwhite(s2), loc2)) => 
			  	(Pwhite(s1 ^ s2), combineloc(loc1, loc2))
			| ((Pempty, loc1), (Pempty, loc2)) => 
				(Pempty, combineloc(loc1, loc2))
			| ((tk1, loc1), (tk2, loc2)) =>
				(Pstring(tokenToString(tk1) ^ tokenToString(tk2)), combineloc(loc1, loc2))
	    end
	 (*the two token lists are supposed to be of equal length*)
	 fun mergetoklist (tl1: LToken list, tl2: LToken list): LToken list =
			case tl2 of 
			nil => tl1
			| _ => ListPair.mapEq mergetok (tl1, tl2)
			handle UnequalLengths => (ListPair.map mergetok (tl1, tl2))

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
  	(cmos, Pstruct(a, newtylist))
    end
| _ => (cmos, ty)
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
(* rule to convert a normal Parray to a refined RArray *)
and refine_array ty = 
	case ty of 
	(* 1st case is looking at the Parray itself *)
	Parray(aux, {tokens, lengths, first, body, last}) =>
		let
(*
		val _ = (print "trying to refine array in struct \n"; printTy ty) 
*)
		fun getlen (lens, x) = 
			case lens of 
			l::tail => if (l = x) then getlen(tail, x)
				   else NONE
			| nil => SOME(IntConst(Int.toLarge(x)))
		val lens = (#1 (ListPair.unzip(lengths)))		
		val lenop = getlen(lens, hd lens)
		fun isStruct ty = case ty of (Pstruct(_)) => true | _ => false
		fun is_base ty' = case ty' of 
			Base _ => true 
			| RefinedBase _ => true
			| _ => false
		fun firstEle(ty) = 
		  case ty of 
		  Pstruct(aux, tylist) => List.hd tylist
		  | _ => raise TyMismatch
		fun lastEle(ty) = 
		  case ty of 
		  Pstruct(aux, tylist) => List.last tylist
		  | _ => raise TyMismatch
		fun droplast(ty) = 
		  case ty of 
		  Pstruct({label=SOME(id),... }, tylist) => 
			(case (length tylist) of 
			 0 => raise Size
			| 1 => Base(mkTyAux1(0, id), nil)
			| 2 => (hd tylist)
			| _ => let
				val newtylist = List.take(tylist, (length tylist) -1)	
			       in
				Pstruct(mkTyAux1(minCoverage(newtylist), id), newtylist)
			       end
			)
		  | _ => raise TyMismatch
(*
		fun dropfirst(ty) = 
		  case ty of 
		  Pstruct(aux, tylist) => Pstruct(aux, List.drop(tylist, 1))
		  | _ => raise TyMismatch
*)
		fun addtohead(ty, newty) =
		  case ty of
		  Pstruct({coverage, label=SOME id, ...}, tylist) => 
				Pstruct(mkTyAux1(Int.min(coverage, getCoverage newty), id), 
					[newty]@tylist)
		  |Punion({label=SOME id, ...}, tylist) => 
				let val newtylist = map (fn oldty => addtohead(oldty, newty)) tylist
				in 
				Punion (mkTyAux1(sumCoverage newtylist, id), newtylist)
				end
		  |RefinedBase({coverage, ...}, _, _) => Pstruct(mkTyAux(Int.min(coverage, 
				getCoverage(newty))), [newty, ty])
		  |Base({coverage, ...}, _) => Pstruct(mkTyAux(Int.min(coverage, getCoverage(newty))), 
							[newty, ty])
		  | _ => (raise TyMismatch)

(*
		fun addtotail(ty, newty) =
		  (*Note: aux is wrong here *)
		  case ty of
		  Pstruct(aux, tylist) => Pstruct(aux, tylist@[newty])
		  |RefinedBase(aux, _, _) => Pstruct(aux, [ty, newty])
		  |Base(aux, _) => Pstruct(aux, [ty, newty])
		  | _ => raise TyMismatch
*)

	  	fun findRefined ty =
		  (*funtion to find the first base or refine type and convert it to refined type *)
			case ty of
			  Pstruct(_, tylist) => findRefined (hd tylist)
			| RefinedBase(_, refined, _) => SOME(refined)
			| Base(_, ltokens) => ltokenlToRefinedOp ltokens
			| _ => NONE
		fun combineRefined (ref1, ref2) =
			case (ref1, ref2) of
			(StringME(s), Int(_)) => SOME(StringME(substring(s, 0, size(s)-1)
						^"[0-9]*/"))
			| (StringME(s), IntConst(_)) => SOME(StringME(substring(s, 0, size(s)-1)^
							"[0-9]*/"))
			| (StringME(s), StringME(t)) => SOME(StringME(substring(s, 0, size(s)-1)^
						      substring(s, 1, size(s)-1)))
			| (StringConst(s), StringConst(t)) => SOME(StringConst(s^t))
			| (StringConst(s), Int(_)) => SOME(StringME("/"^s^"[0-9]*/"))
			| (StringConst(s), IntConst(_)) => SOME(StringME("/"^s^"[0-9]*/"))
			| (Enum(l1), Enum(l2)) => SOME(Enum(l1@l2))
			| _ => NONE
				
		fun getRefine(ty) = case ty of RefinedBase(_, r, _) => SOME(r) 
					| Base (_, tl) => ltokenlToRefinedOp tl
					| _ => NONE
		fun isEmpty(ty) = case ty of 
					RefinedBase(_, StringConst(""), _) => true
					| Base(_, tkl) =>
						( 
						  case (hd tkl) of 
							(Pempty, _) => true
							| _ => false
						)
					| _=> false

		(* if the firsttail = body tail, then this is a possible separator.
		   if the stripped first is part of body and last is part of body, then
		   the separator is confirmed, and the first and last can be obsorbed
		   into the body. if either stripped first or the stripped last is part 
		   of the body, then the separator is confirmed and either the first or the
		   the last is absorbed into the body, and the other one is pushed out 
		   of the array.
		   if none of the first and the last is part of body, then no separator is
		   defined and both first and last are pushed out of the array. *)

		(*the separator should be a refinedbase ty in the last position
		 of the first and body tys, or no separator if the two elements are not
		 equal, or if the body is the same as the tail or the tail is empty *)
		fun getSepTerm(first, body, last)=
		let
			val bodyhd = getRefine(firstEle(body))	
			val bodytail = getRefine(lastEle(body))
			val firsttail = if (isStruct(first)) then getRefine(lastEle(first))
					else getRefine(first) (*assume it's a base itself*)
			val lasthd= if (isStruct(last)) then getRefine(firstEle(last))
					else getRefine(last)
		in
			if (isEmpty(last)) (*no sep and terminator is outside*)
			then if describedBy(first, body) then
					(NONE, NONE, NONE, SOME(mergeTyInto(first, body)), NONE)
				else
					(NONE, NONE, SOME first, SOME body, NONE)
			else (* with possible sep and possible term inside last *)
			     (* two cases: first = body or first != body *)
			  let
			     val firsteqbody = describedBy(first, body)
			     val lasteqbody = describedBy(last, droplast(body)) 
			     val withSep = refine_equal_op(firsttail, bodytail)
(*
			     val _ = (if firsteqbody then print "true " else print "false ";
				   	if lasteqbody then print "true " else print "false ";
				   	if withSep then print "true\n" else print "false\n")
*)
			  in
			     case (firsteqbody, lasteqbody, withSep) of 
				(true, true, true) => 
					let 
					  val body' = mergeTyInto(droplast(first), droplast(body))
					  val body'' = mergeTyInto(last, body')
					in
					(bodytail, NONE, NONE, SOME(body''), NONE)
					end
				| (true, true, false) => (NONE, NONE, NONE,
					SOME(mergeTyInto(last, 
						mergeTyInto(droplast(first), droplast(body)))),
					NONE)
				| (true, false, _) => (NONE, NONE, NONE, 
					SOME(mergeTyInto(first, body)), SOME last)
				| (false, true, _) => (bodytail, NONE, SOME first,
					SOME(mergeTyInto(last, droplast(body))), NONE)
				| (_, _, _) => (NONE, NONE, SOME first, SOME body, SOME last)
			  end
		end handle TyMismatch => (NONE, NONE, SOME first, SOME body, SOME last)
	in
		  let 
		    val (sepop, termop, firstop, bodyop, lastop) = getSepTerm(first, body, last)
		    val newty = 
			case (firstop, bodyop, lastop) of 
			 (NONE, SOME(body'), NONE) => 
				RArray(aux, sepop, termop, body', lenop, lengths)
			|(NONE, SOME(body'), SOME(last')) =>
			  	Pstruct(mkTyAux(#coverage aux), 
				[RArray(aux, sepop, termop, body', lenop, lengths), last'])
			|(SOME(first'), SOME(body'), NONE) =>
			  	Pstruct(mkTyAux(#coverage aux), 
				[first', RArray(aux, sepop, NONE, body', lenop, lengths)])
			|(SOME(first'), SOME(body'), SOME(last')) =>
			  	Pstruct(mkTyAux(#coverage aux), 
			    	[first', RArray(aux, sepop, termop, body', lenop, lengths), last'])
			| _ => ty
(*
		    val _ = (print "Done refining array in struct to:\n"; printTy newty)  
*)
		  in
		 	newty
		  end
	end 
	| Pstruct(a, tylist) =>
		let 
(*
		  val _ = (print "trying to refine array in struct \n"; printTy ty)
*)
		  fun findRefined ty =
		  (*funtion to find the first base or refine type and convert it to refined type *)
			case ty of
			  Pstruct(_, tylist) => findRefined (hd tylist)
			| RefinedBase(_, refined, _) => SOME(refined)
			| Base(_, ltokens) => ltokenlToRefinedOp ltokens
			| _ => NONE

		  fun updateTerm (arrayty, termop) =
			case (arrayty) of
				RArray(a, sep, term, body, len, lengths)=>
					RArray(a, sep, termop, body, len, lengths)
			| _ => raise TyMismatch
		  fun updateArray tylist newlist =
			case tylist of
			nil => newlist
			| ty::tail =>
				case ty of RArray (_, _, NONE, _, _, _) => 
				(
				  case tail of 
				  nil => newlist@tylist
				  | _ =>
				  	let
				  	val nextRefined= findRefined(hd tail)	
				  	val newArray = updateTerm(ty, nextRefined)
					in newlist@[newArray]@tail
				  	end
				)
				| _ => updateArray tail newlist@[ty] 

		  val tylist' = updateArray tylist nil
(*
		  val _ = (print "Done refining array in struct to:\n"; printTy (Pstruct(a, tylist')))
*)
		in
		  Pstruct(a, tylist')
		end
	|_ => ty

(* post constraint rules, these require the cmap to be filled  and the 
data labeled *)

(* a unique Base type becomes a constant type *)
(* Notice that the unique constraint has not been taken away from the LabelMap *)
and uniqueness_to_const cmos ty =
case ty of
  Base({coverage, label=SOME id, ...}, tokens) => 
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
		  SOME(PbXML(x, y)) => 
			(
				newcmos, 
				RefinedBase((mkTyAux1(coverage, id)), 
				StringConst("<"^x^" "^y^">"), tokens)
			)
		| SOME(PeXML(x, y)) => 
			(
				newcmos, 
				RefinedBase((mkTyAux1(coverage, id)), 
				StringConst("</"^x^" "^y^">"), tokens)
			)
         	| SOME(Pint(x)) => 
			(
				newcmos, 
				RefinedBase((mkTyAux1(coverage, id)), 
				IntConst(x), tokens)
			)
		| SOME(Pstring(x)) => 
			(
				newcmos, 
				RefinedBase((mkTyAux1(coverage, id)), 
				StringConst(x), tokens)
			)
		| SOME(Ptime(x)) => 
			(
				newcmos, 
				RefinedBase((mkTyAux1(coverage, id)), 
				StringConst(x), tokens)
			)
		| SOME(Pdate(x)) => 
			(
				newcmos, 
				RefinedBase((mkTyAux1(coverage, id)), 
				StringConst(x), tokens)
			)
		| SOME(Pip(x)) => 
			(
				newcmos, 
				RefinedBase((mkTyAux1(coverage, id)), 
				StringConst(x), tokens)
			)
		| SOME(Phostname(x)) => 
			(
				newcmos, 
				RefinedBase((mkTyAux1(coverage, id)), 
				StringConst(x), tokens)
			)
		| SOME(Ppath(x)) => 
			(
				newcmos, 
				RefinedBase((mkTyAux1(coverage, id)), 
				StringConst(x), tokens)
			)
		| SOME(Purl(x)) => 
			(
				newcmos, 
				RefinedBase((mkTyAux1(coverage, id)), 
				StringConst(x), tokens)
			)

		| SOME(Pwhite(x)) => 
			(
				newcmos, 
				RefinedBase((mkTyAux1(coverage, id)), 
				StringConst(x), tokens)
			)
		| SOME(Other(x)) => 
			(
				newcmos, 
				RefinedBase((mkTyAux1(coverage, id)), 
				StringConst(Char.toString(x)), tokens)
			)
		| SOME(Pempty) => 
			(
				newcmos, 
				RefinedBase((mkTyAux1(coverage, id)), 
				StringConst(""), tokens)
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
	(* function to test if a base value with a specific id exists in a ty list*) 
	fun existsbase(tlist, id) = 
		case (tlist) of
			Base(a, _)::tail => if Atom.same(id, getLabel(a)) then true
							else existsbase(tail, id)
			| RefinedBase(a, _, _)::tail => if Atom.same(id, getLabel(a)) then true
							else existsbase(tail, id)
			| ty::tail => if (Atom.same(getLabel(getAuxInfo(ty)), id)) then false
					else existsbase(tail, id)
			| nil => false
	
	(* test if a sum is a switched sum depending on some other id*)
	fun is_switch(cmos, id) = 
	  case LabelMap.find(cmos, id) of  
      		SOME consts => 
        	  let 
			fun cost_switch c =
				case c of
					NONE => some(Int.maxInt)
					| SOME(Switched (_, mappings)) => (length mappings)
					|_ => raise TyMismatch
			fun find_switch(clist, newclist, cur_cheapest) =
      	  			case clist of 
				  Switched (ids, mappings) :: t => 
				    (* we are only interested in 1-1 mapping in switched *)
				    (* we also use the "cheapest" switch of all the switches and
					delete all the more expensive 1-1 switches *)
				    if length(ids) = 1 andalso 
					length(#1 (hd mappings))=1 andalso existsbase(tylist, hd ids)
				    then 
					(
					if (cost_switch(SOME(Switched (ids, mappings)))< 
						cost_switch(cur_cheapest))
					then find_switch (t, newclist, SOME(Switched(ids, mappings)))
					else find_switch (t, newclist, cur_cheapest)
					)
				    else find_switch (t, (newclist@[Switched(ids, mappings)]), cur_cheapest)
      	  			  | h :: t => find_switch (t, (newclist@[h]), cur_cheapest)
      	  			  | nil => (case cur_cheapest of 
						SOME(Switched(ids, mappings)) => 
							(SOME(ids, mappings), newclist)
						| _ => (NONE, newclist)
					   )
			val (someidmappings, newconsts) = find_switch(consts, nil, NONE)
			val (newcmos, _) = LabelMap.remove(cmos, id)
	    		val newcmos = LabelMap.insert(newcmos, id, newconsts)
		  in
			(someidmappings, newcmos)
		  end 
    	 	| NONE => (NONE, cmos) 
			
	fun to_switch (ty, id, mappings)=
	(* convert a union ty to a Switch type if possible given an id 
	   from a switch variable and mappings of a list of tuples 
	  ([token option], token option) *)
	  case ty of
	    Punion(aux, tlist) =>
	  	let
		    fun getrefine (index, mappings) = 
		    (*given an index, give a list of refined values that points to this index from the mapping*)
		      case mappings of 
			([SOME tok1], SOME(Pint i))::tail => 
				if (i=index) then tokentorefine(tok1)::getrefine(index, tail)
				else getrefine (index, tail)
		      	| _::tail => getrefine(index, tail)
		      	| nil => nil
		    (*assume index starts from 1 for the tylist branches*)
		    fun   gen_ref_ty_list (_, nil, _) = nil
			| gen_ref_ty_list (mappings, head::tail, index) =
			(case getrefine(index, mappings) of
			    nil => nil(* returns immediately if no refine is found *)
			  | refined => if length(refined) = 1 then
						((hd refined), head):: gen_ref_ty_list(mappings, tail, index+1)
					else
						(Enum(refined), head):: gen_ref_ty_list(mappings, tail, index+1)
			)
		    val refine_ty_list = gen_ref_ty_list (mappings, tlist, 1)
		in
		    if (length refine_ty_list = length tlist) 
		    then (Switch (aux, id, refine_ty_list))
		    else ty
		end
	   | _ => ty
		    
	fun rewrite_switch (cmos, tlist) =
	    case tlist of 
		h::rest => 
		(
		case h of 
			Punion(a, sumlist) => 
			  let 
			    val (c, newcmos)  = is_switch(cmos, some(#label a)) 
			  in 
			    case c of 
				SOME ([id], mappings) =>
					(
					  let 
						val (newcmos, rest') = rewrite_switch(newcmos, rest)
					  in
						(newcmos, to_switch(h, id, mappings)::rest')
					  end
					)
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
and enum_range_to_refine cmos ty = 
  case ty of                  
    Base({coverage, label=SOME(id), ...}, b) =>         
    (case LabelMap.find(cmos,id) of 
      SOME consts =>    
        let             
                fun check_enum list newconsts=  
                  case list of (EnumC set) :: t =>
                      let
                        val items = BDSet.listItems set
                        val refs = map tokentorefine items
                        val ty' = (if length(refs)=1 then 
				RefinedBase(mkTyAux1(coverage, id), hd refs, b)
				else
				(if (length(refs)=0) then ty
				else RefinedBase(mkTyAux1(coverage, id), Enum refs, b)
				)
				)
(*                     	val _ = print ("ENUM: " ^ (TyToString ty')^"\n") *)
                      in
                        (ty', newconsts@t)
                      end
		  | (Range(min,max)):: t => (RefinedBase(mkTyAux1(coverage, id), 
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
			remove_nils,
		  	unused_branches,
			refine_array
		]
  val post_constraint_rules : post_reduction_rule list =
		[ 
		  uniqueness_to_const, 
		  adjacent_consts,
		  enum_range_to_refine,
		  sum_to_switch
		]
  (* generate the lust of rules *)
  val (mode, cmap) = case const_info_op of 
      SOME (cmap) => (1, cmap)
    | NONE => (0, LabelMap.empty)
  (* returns a new cmap after reducing all the tys in the list and a new tylist *)
  fun mymap f mode cmap tylist newlist =
	case tylist of 
		ty::tail => let 
				val(cmap', ty') = f mode cmap ty
			    in	mymap f mode cmap' tail (newlist@[ty'])
			    end
		| nil => (cmap, newlist)

  (*reduce a ty and returns the new constraint map and the new ty *)
  (* mode = 0: pre_constraint; mode = 1: post_constraint *)
  fun reduce' mode cmap ty =
    let 
      	(* go bottom up, calling reduce' on children values first *)
      	val (newcmap, reduced_ty) = 
			case ty of
			  Pstruct (a, tylist) => let 
				val (cmap', tylist') = mymap reduce' mode cmap tylist nil
				in (cmap', Pstruct (a, tylist'))
				end
			| Punion (a, tylist) => let
				val (cmap', tylist') = mymap reduce' mode cmap tylist nil
				in (cmap', Punion(a, tylist'))
				end
			| Parray (a, {tokens, lengths, first, body, last}) => 
				let
				val (cmap1, firstty) = reduce' mode cmap first
				val (cmap2, bodyty) = reduce' mode cmap1 body 
				val (cmap3, lastty) = reduce' mode cmap2 last 
				in
				(cmap3, Parray(a, {tokens=tokens, lengths=lengths, 
				first=firstty,
				body= bodyty,
				last=lastty}))
				end
			| Base b => (cmap, Base b)
			| TBD b => (cmap, TBD b)
			| Bottom b => (cmap, Bottom b)
			| RefinedBase b => (cmap, RefinedBase b)
                        | Switch (a, id, pairs) =>  
                                let
                                val (refs, tylist) = ListPair.unzip(pairs)
                                val (cmap', tylist') = mymap reduce' mode cmap tylist nil
                                in (cmap', Switch (a, id, ListPair.zip(refs, tylist')))
                                end
                        | RArray (a, sep, term, body, len, lengths) => 
                                let
                                val (cmap', body') = reduce' mode cmap body
                                in
                                (cmap', RArray (a, sep, term, body', len, lengths))
                                end

	  fun iterate cmap ty = 
	  let
	    (* calculate the current cost *)
	    val cur_cost = cost cmap ty
	    (* apply each rule to the ty *)
	    val cmap_ty_pairs = if mode=0 then
					map(fn x => (cmap, x ty)) pre_constraint_rules
				else
					map (fn x => x cmap ty) post_constraint_rules 
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
(*val cbefore = cost cmap ty *)
  val (cmap', ty') = reduce' mode cmap ty 
(*  val cafter = cost cmap' ty'*)
(*  val _ = print ("Before:" ^ (Int.toString cbefore) ^ " After:" ^ (Int.toString cafter) ^ "\n") *)
in
  ty'
end 

end
