(* 
Zach DeVito
Kenny Zhu
Reduce implements the refinement system for Tys
*)
structure Reduce = struct
open Common
open Types
open Model
exception TyMismatch
exception InvalidToken
exception Unexpected

(* a table to record which base types are Refinable*)
fun refineableBase token = 
  case token of 
	  PbXML _ => true
	| PeXML _ => true
	| Pint _ => true
	| Pstring _ => true
	| Pwhite _ => true
	| Other _ => true
	| _ => false
fun enumerableBase token = 
  case token of 
	  PbXML _ => true
	| PeXML _ => true
	| Pint _ => true
	| Pstring _ => true
	| Other _ => true
	| _ => false
	
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
				| FloatConst _ => 1
				| StringConst _ => 1
				| Enum l => length(l)
				| LabelRef _ => 1
				| Blob _ => 1
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
		| Poption (a, ty) => (cost const_map ty) + 3 
	in (ty_cost ty) + (total_const_cost ty) + 1 (* every constraint is counted towards cost *)
  end

fun score ty =
	let
		val comps = getComps (measure ty)
		val rawcomp = combine (#tc comps) (#dc comps)
	in (toReal rawcomp)
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

fun remove_degenerate_list1 cmap ty =
case ty of
  Punion(a, h :: nil) => (cmap, h)
| Punion (a, nil) => (cmap, Base(a, nil))
| Pstruct (a, h :: nil) => (cmap, h)
| Pstruct (a, nil) => (cmap, Base(a, nil))
| Switch(a, id, nil) => (cmap, Base(a, nil))
| Parray(a, {tokens, lengths, first, body, last}) => 
	if lengths=nil then (cmap, Base(a, nil)) else (cmap, ty) 
| RArray(a, _, _, _, _, nil) => (cmap, Base(a, nil)) 
| _ => (cmap, ty)


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
  Pstruct (a, nil) => Base(a, nil)
  | Pstruct (a, tylist) =>
    let val tylist' =  List.filter (fn x => case x of
                        Base(_, nil) => false
                        | Base (_, ((Pempty, _)::_)) => false
                        | RefinedBase(_, _, nil) => false
                        | _ => true ) tylist
    in
        case tylist' of
          nil => hd tylist
        | _ => Pstruct(a, tylist')
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
(* elements of a sum are check to see if they share a common prefix (ie tuples with
a common prefix, or a common postfix) these elements are then brought out of a sum
and a tuple is created *)
(* TODO: the token coverage in the aux info may not be correct after this operation *)
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
  	val rem_tups = map(fn (a, tys) => case tys of
				nil => genEmptyBase a (#coverage a)
				| t::nil => t
				| _ => Pstruct(a, tys)
			      ) (ListPair.zip(auxlist, remaining))
	val unionTys = case length rem_tups of
			0 => nil
			| 1 => rem_tups
			(* | _ => [Punion(a, rem_tups)] *)
			| _ => [union_to_optional (Punion(a, rem_tups))]
  	val newty = case (cpfx, csfx) of
  	  (h::t, _) => Pstruct (mkTyAux (#coverage a), 
				cpfx @ unionTys @ csfx)
  	| (_,h::t) => Pstruct (mkTyAux (#coverage a), 
				cpfx @ unionTys @ csfx)
  	| (nil,nil) => Punion (a, tylist)
  in newty
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
		fun getNewLabel x = SOME ( getLabel ( { coverage = x
                                                      , label=NONE
                                                      , tycomp = zeroComps
                                                      }
                                                    )
                                         )
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
					val c   = #coverage aux
					val l   = #label aux
					val tc  = #tycomp aux
					val aux1 = {coverage=1, label=getNewLabel 1, tycomp=tc }
					val aux2 = {coverage=c-1, label=l, tycomp = tc }
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
			val a1 = {coverage=1, label=getNewLabel 1, tycomp = zeroComps }
			val a2 = {coverage=overallCoverage-1, 
				label=getNewLabel 1, tycomp = zeroComps }
			val newty = Punion(a, [Pstruct(a1, tys1), Pstruct(a2, tys2)])
			val _ = (print "Cost for ty: "; print (Real.toString(score ty)))
			val _ = (print "\nCost for newty: "; print (Real.toString(score newty)))
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
		case (t1, t2) of 
			((Pwhite(s1), loc1), (Pwhite(s2), loc2)) => 
			  	(Pwhite(s1 ^ s2), combLoc(loc1, loc2))
			| ((Pempty, loc1), (Pempty, loc2)) => 
				(Pempty, combLoc(loc1, loc2))
			| ((tk1, loc1), (tk2, loc2)) =>
				(Pstring(tokenToRawString(tk1) ^ tokenToRawString(tk2)), 
				combLoc(loc1, loc2))
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
(* rule to convert a normal Parray to a refined RArray *)
and refine_array ty = 
	case ty of 
	(* 1st case is looking at the Parray itself *)
	Parray(aux, {tokens, lengths, first, body, last}) =>
		let
(*
		val _ = (print "trying to refine array \n"; printTy (measure ty)) 
*)
		fun getlen (lens, x) = 
			case lens of 
			l::tail => if (l = x) then getlen(tail, x)
				   else NONE
			| nil => SOME(IntConst(Int.toLarge(x)))
		val lens = (#1 (ListPair.unzip(lengths)))		
		val lenop = getlen(lens, hd lens)
		fun isStruct ty = case ty of 
			(Pstruct(_)) => true 
			| Poption (a, ty') => isStruct ty'
			| _ => false
		fun is_base ty' = case ty' of 
			Base _ => true 
			| RefinedBase _ => true
			| _ => false
		fun firstEle(ty) = 
		  case ty of 
		  Pstruct(aux, tylist) => List.hd tylist
		  | Poption(_, ty') => firstEle ty'
		  | _ => raise TyMismatch
		fun lastEle(ty) = 
		  case ty of 
		  Pstruct(aux, tylist) => List.last tylist
		  | Poption(_, ty') => lastEle ty'
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
		  | Poption (a, ty') => Poption (a, droplast ty')
		  | _ => raise TyMismatch
(*
		fun dropfirst(ty) = 
		  case ty of 
		  Pstruct(aux, tylist) => Pstruct(aux, List.drop(tylist, 1))
		  | _ => raise TyMismatch
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
			| Poption (_, ty') => findRefined ty'
			| RefinedBase(_, refined, _) => SOME(refined)
			| Base(_, ltokens) => ltokenlToRefinedOp ltokens
			| _ => NONE
(*
		fun combineRefined (ref1, ref2) =
			case (ref1, ref2) of
			(StringME(s), Int(_)) => SOME(StringME(substring(s, 0, size(s)-1)
						^"[0-9]*/"))
			| (StringME(s), IntConst(_)) => SOME(StringME(substring(s, 0, size(s)-1)^
							"[0-9]*/"))
			| (StringME(s), StringME(t)) => SOME(StringME(substring(s, 0, size(s)-1)^
						      substring(s, 1, size(s)-1)))
			| (StringConst(s), StringConst(t)) => SOME(StringConst(s^t))
			| (StringConst(s), Int(_)) => SOME(StringME("/"^ (String.toCString s) ^"[0-9]*/"))
			| (StringConst(s), IntConst(_)) => SOME(StringME("/"^ (String.toCString s) ^
								"[0-9]*/"))
			| (Enum(l1), Enum(l2)) => SOME(Enum(l1@l2))
			| _ => NONE
*)				
		fun getRefine(ty) = case ty of RefinedBase(_, r, _) => SOME(r) 
					| Base (_, tl) => ltokenlToRefinedOp tl
					| _ => NONE
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
		(* returns (sep option, term option, newfirst, newbody, newlast) *)
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
				  let
					val first' = reIndexRecNo first (getCoverage body)
				  in
					(NONE, NONE, NONE, SOME(mergeTyInto(first', body)), NONE)
				  end
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
					  val first' = reIndexRecNo (droplast first) (getCoverage body)
					  val body' = mergeTyInto(first', droplast(body))
					  val last' = reIndexRecNo last (getCoverage body')
					  val body'' = mergeTyInto(last', body')
					in
					  (bodytail, NONE, NONE, SOME(body''), NONE)
					end
				| (true, true, false) => 
					let
					  val first' = reIndexRecNo (droplast first) (getCoverage body)
					  val body' = mergeTyInto(first', (droplast body))
					in
					  (NONE, NONE, NONE, SOME body', SOME last)
					end
				| (true, false, _) => (NONE, NONE, NONE, 
					SOME(mergeTyInto((reIndexRecNo first (getCoverage body)), body)), 
					SOME last)
				| (false, true, _) => (bodytail, NONE, SOME first,
					SOME(mergeTyInto((reIndexRecNo last (getCoverage body)), 
					droplast(body))), NONE)
				| (_, _, _) => (NONE, NONE, SOME first, SOME body, SOME last)
			  end
		end handle TyMismatch => (NONE, NONE, SOME first, SOME body, SOME last)
	in
		  let 
		    val (sepop, termop, firstop, bodyop, lastop) = getSepTerm(first, body, last)
		    val newty = 
			(* NOTE: update of the lengths only gives a safe upper bound, i.e.
			the length of each record cannot be longer than the updated length, 
			it's actually not accurate *)
			case (firstop, bodyop, lastop) of 
			 (NONE, SOME(body'), NONE) => 
				RArray(aux, sepop, termop, body', lenop, lengths)
			|(NONE, SOME(body'), SOME(last')) =>
			  	Pstruct(mkTyAux(#coverage aux), 
				[RArray(aux, sepop, termop, body', lenop, lengths), 
					last'])
			|(SOME(first'), SOME(body'), NONE) =>
			  	Pstruct(mkTyAux(#coverage aux), 
				[first', RArray(aux, sepop, NONE, body', lenop, lengths)])
			|(SOME(first'), SOME(body'), SOME(last')) =>
			  	Pstruct(mkTyAux(#coverage aux), 
			    	[first', RArray(aux, sepop, termop, body', lenop, lengths), last'])
			| _ => ty
(*
		    val _ = (print "Done refining array to:\n"; printTy (measure newty))  
*)
		  in
		 	newty
		  end
	end 
	| Pstruct(a, tylist) =>
		let 
(*
		  val _ = (print "trying to refine array in struct \n"; printTy (measure ty))
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
			| t::tys =>
				case t of RArray (_, _, NONE, _, _, _) => 
				(
				  case tys of 
				  nil => newlist@[t]
				  | _ =>
				  	let
				  	val nextRefined= findRefined(hd tys)	
				  	val newArray = updateTerm(t, nextRefined)
					in newlist@[newArray]@tys
				  	end
				)
				| _ => updateArray tys (newlist@[t]) 

		  val tylist' = updateArray tylist nil
(*
		  val _ = (print "Done refining array in struct to:\n"; printTy (measure (Pstruct(a, tylist'))))
*)
		in Pstruct(a, tylist')
		end
	|_ => ty

and struct_to_array ty =
(*this rule converts a struct with repeated content to an fixed length RArray*)
(*TODO: it is possible to convert a subsequence of the tylist into an RArray
	but this could be expensive *)
  case ty of 
    Pstruct (a, tylist) =>
	if length tylist <3 then ty
	else
	  let
	    (*function to takes a tylist and divides it into 
		size n chunks and merges the chunks together, the resulting
		SOME tylist plus SOME sep is of size n or NONE if not possible to do that *)
(*
	    val _ = (print "Before:\n";printTy (measure ty))
*)
	    fun tylistEqual (tys1, tys2) =
		let
		  val pairs = ListPair.zipEq (tys1, tys2)
		  val equal = foldl myand true (map (fn (x, y) => ty_equal (1, x, y)) pairs)
		in
		  equal
		end handle UnequalLengths => false
		  	
	    fun getRefine(ty) = case ty of RefinedBase(_, r, _) => SOME(r) 
					| Base (_, tl) => ltokenlToRefinedOp tl
					| _ => NONE
	    fun divMerge (tylist:Ty list) (n : int) (newlist: Ty list) = 
	      if ((length tylist) mod n)>0 andalso ((length tylist) mod n) < (n-1) 
	      then NONE
	      else
		(* last iteration possibly with a sep *)
		if (length tylist <>0) andalso (length tylist) = (length newlist)-1  
		then
		  let
			val refop = getRefine (List.last newlist)
			val body = List.take (newlist, n-1)
		  in
			case refop of
			NONE => NONE
			| SOME r => 
			  if tylistEqual (body, tylist) then 
			    SOME ((map mergeTyForArray (ListPair.zip (body, tylist))), SOME r)
			  else NONE
		  end
		else case tylist of 
		  nil => SOME (newlist, NONE)
		  | _ =>
		    let
		      val first = List.take (tylist, n)
		      val tail = List.drop (tylist, n)
		    in
		      case newlist of
			(*at the begining, the first chunk is reindexed at 0 *)
			nil => divMerge tail n (map (fn t => reIndexRecNo t 0) first)
			| _ =>
		          if tylistEqual (newlist, first) then 
				divMerge tail n (map mergeTyForArray (ListPair.zip (newlist, first))) 
			  else NONE
		     end handle Subscript => NONE
	    fun try m n = 
		if (m>n) then NONE
	        else 
		let
		  val listop = divMerge tylist m nil 
		in
		  case listop of 
			NONE => try (m+1) n
			| _ => listop
		end
	    val tysop = try 1 ((length tylist) div 3)
	  in
	    case tysop of
		NONE => ty
		| SOME (tylist', NONE) => 
		  let
		    (* no sep *)
		    (* get the map of recNos *)
		    val recNoMap = insertToMap ty IntMap.empty
		    val len = (length tylist) div (length tylist')
		    val lens = map (fn (r, _) => (len, r)) (IntMap.listItemsi recNoMap)
		    val newty = RArray (a, NONE, NONE, Pstruct(mkTyAux (getCoverage (hd tylist')), tylist'), 
			(SOME (IntConst (Int.toLarge len))), lens)
(*
	    	    val _ = (print "After:\n"; printTy newty)
*)
		  in newty 
		  end 
		| SOME (tylist', SOME r) => 
		  let
		    (* with sep *)
		    (* get the map of recNos *)
		    val recNoMap = insertToMap ty IntMap.empty
		    val len = (length tylist + 1) div ((length tylist') + 1)
		    val lens = map (fn (r, _) => (len, r)) (IntMap.listItemsi recNoMap)
		    val newty = RArray (a, SOME r, NONE, Pstruct(mkTyAux (getCoverage (hd tylist')), 
			tylist'), 
			(SOME (IntConst (Int.toLarge len))), lens)
(*
	    	    val _ = (print "After:\n";printTy (measure newty))
*)
		  in newty 
		  end 
	  end
    | _ => ty

(* find negative number (both int and float)  rule (Phase one rule) *)
and find_neg_num ty =
	case ty of
	Pstruct (a, tylist) =>
	let
	  fun isPunctuation ty =
		case ty of
		  Base (_, ((Other (#"-"), _)::_)) => false
		| Base (_, ((Other (#"+"), _)::_)) => false
		| Base (_, ((Other _, _)::_)) => true	
		| Base (_, ((Pwhite _, _)::_)) => true	
		| _ => false
	  (*the tl1 represents a subset of records of tl2*)
	  fun mergetoklist (tl1: LToken list, tl2: LToken list): LToken list =
	  let
	    fun insertNumToMap (ltoken:LToken, recMap) = IntMap.insert (recMap, (#recNo (#2 ltoken)), ltoken)
	    fun insertSignToMap ((Other x, (loc:location)), recMap) =
		let
		  val tokOp = IntMap.find (recMap, (#recNo loc))
		in
		  case tokOp of
			SOME (Pint (i, s), loc1) =>
				IntMap.insert (recMap, (#recNo loc), (Pint(~i, "-"^s), combLoc(loc, loc1)))
			| SOME (Pfloat (i, f), loc1) =>
				IntMap.insert (recMap, (#recNo loc), (Pfloat("-"^i, f), combLoc(loc, loc1)))
			| _ => (print "RecNum doesn't match!" ; raise TyMismatch)
		end
	    val tokenmap= foldl insertNumToMap IntMap.empty tl2
	    val tokenmap = foldl insertSignToMap tokenmap tl1
	  in
	    IntMap.listItems tokenmap
	  end

	  fun combineTys (Base (a1, tl1), Base(a2, tl2)) = 
			Base (a2, mergetoklist (tl1, tl2))  
	     | combineTys (Poption (_, Base(a1, tl1)), Base(a2, tl2)) =
			Base (a2, mergetoklist (tl1, tl2))
		
	  fun matchPattern pre tys =
		case tys of 
		   nil => pre
		  | (ty1 as Base(a1, (Other (#"-"), _)::_))::((ty2 as Base(a2, (Pint _, _)::_)) :: post) => 
		     if (length pre = 0) then (matchPattern [combineTys (ty1, ty2)] post)
		     else if isPunctuation (List.last pre) then (matchPattern (pre@[combineTys (ty1, ty2)]) post)
		     else matchPattern (pre@[ty1, ty2]) post
		  | (ty1 as Base(a1, (Other (#"-"), _)::_))::((ty2 as Base(a2, (Pfloat _, _)::_)) :: post) => 
		     if (length pre = 0) then (matchPattern [combineTys (ty1, ty2)] post)
		     else if isPunctuation (List.last pre) then (matchPattern (pre@[combineTys (ty1, ty2)]) post)
		     else matchPattern (pre@[ty1, ty2]) post
		  | (ty1 as Poption(_, Base(a1, (Other (#"-"), _)::_)))::
			((ty2 as Base(a2, (Pint _, _)::_))::post) => 
		     if (length pre = 0) then (matchPattern [combineTys (ty1, ty2)] post)
		     else if isPunctuation (List.last pre) then (matchPattern (pre@[combineTys (ty1, ty2)]) post)
		     else matchPattern (pre@[ty1, ty2]) post
		  | (ty1 as Poption(_, Base(a1, (Other (#"-"), _)::_)))::
			((ty2 as Base(a2, (Pfloat _, _)::_))::post) => 
		     if (length pre = 0) then (matchPattern [combineTys (ty1, ty2)] post)
		     else if isPunctuation (List.last pre) then (matchPattern (pre@[combineTys (ty1, ty2)]) post)
		     else matchPattern (pre@[ty1, ty2]) post
		  | x::rest => matchPattern (pre@[x]) rest
	in Pstruct(a, matchPattern nil tylist)
	end
	| _ => ty

(* int to float rule (Phase one rule)
several scenarios:
tys = Pint . Pint => Pfloat
tys = Pint  Poption (. Pint) => Pfloat
[not dot] tys [not dot]
Pint + Pfloat => Pfloat
*)
(*TODO: when checking no dot, we are assuming it's a base, it could be more complex than that,
  also, maybe the second case should rewrite to Pfloat + Pint, instead? *)
and to_float ty = 
	case ty of
	Pstruct (a, tylist) =>
	  let
(*
		val _ = (print "before:\n"; printTy (measure ty))
*)
		fun getFloatTokens (tokens1, tokens2) =
		  let
			fun insertIntToMap (ltok, intmap) =
				case ltok of
				(Pint (i, s), (loc:location)) => 
					IntMap.insert(intmap, (#recNo loc), (Pfloat(s, ""), loc)) 
				| _ => (print "Got a different token than Pint for int!"; raise TyMismatch)
			fun insertFracToMap (ltok, intmap) =
				case ltok of
				(Pint (i, s), loc) => 
				  let
					val tokOp = IntMap.find (intmap, (#recNo loc))
				  in
					case tokOp of
					  NONE => intmap
					  | SOME (Pfloat(ipart, _), loc1) => 
						IntMap.insert(intmap, (#recNo loc), 
						(Pfloat(ipart, s), combLoc(loc1, loc)))
					  | _ => raise TyMismatch
				  end
				| _ => (print "Got a different token than Pint for frac!"; raise TyMismatch)
			val tokenmap= foldl insertIntToMap IntMap.empty tokens1
			val tokenmap = foldl insertFracToMap tokenmap tokens2
		  in
			IntMap.listItems tokenmap
		  end
		fun combineTys tys =
		  case tys of
			[Base(a1, intTokList), Base(_, _), Base (_, intTokList1)]=> 
				Base(a1, getFloatTokens(intTokList, intTokList1))
		      |  [Base(a1, intTokList), Poption(_, Pstruct(_, [(Base _), Base(a3, intTokList1)]))]=>
				Base(a1, getFloatTokens(intTokList, intTokList1))
		      | _ => raise TyMismatch
		fun matchPattern pre tys =
			case tys of 
			   nil => NONE
			  | (Base(a1, (Pint _, _)::_))::((Base(a2, (Other (#"."), _)::_))::
				((Base(a3, (Pint _, _)::_)) :: post)) => 
				SOME (pre, List.take(tys, 3), post)
			  | (Base(a1, (Pint _, _)::_))::((Poption(_, Pstruct(_,
				[Base(a2, (Other (#"."), _)::_), Base(a3, (Pint _, _)::_)]))):: post) => 
				SOME (pre, List.take(tys, 2), post)
			  | (Base(a1, (Other (#"."), _)::_))::(x::rest) => 
				matchPattern (pre@(List.take(tys, 2))) rest
			  | x::rest => matchPattern (pre@[x]) rest
			
		(* there can be multiple floats in the same tylist, we are getting all of them *)	
		fun matchAll pre tys = 
		  let 		  
			val listOp = matchPattern nil tys
		  in
			case listOp of
			NONE => pre@tys
			| SOME (pre', tys', post) => matchAll (pre@pre'@[(combineTys tys')]) post
		  end
		val newtylist = matchAll nil tylist
		val newty = if (length newtylist) = 1 then hd newtylist
			    else Pstruct(a, newtylist)
(*
		val _ = (print "New Ty:\n"; printTy (measure newty))
*)
	  in
		newty
	  end
	| Punion (a, [ty1, ty2]) =>
	  let
		fun toFloatTokens ltokens =
		  case ltokens of
		  nil => nil
		  | (Pint (i, s), loc)::tail => (Pfloat (s, "0"), loc)::(toFloatTokens tail)
		  | _ => raise TyMismatch
	  in
		case (ty1, ty2) of 
		   (Base(a1, toks1 as ((Pint _, _)::_)), Base(a2, toks2 as ((Pfloat _, _)::_))) => 
			Base(a, (toFloatTokens toks1)@toks2)
		  | (Base(a1, toks1 as ((Pfloat _, _)::_)), Base(a2, toks2 as ((Pint _, _)::_))) => 
			Base(a, toks1@(toFloatTokens toks2))
		  | _ => ty
	  end
	| _ => ty

(* this rule is used for only one case now: ty1 + Pemty ==> Poption ty1 *)
and union_to_optional ty =
	case ty of 
	Punion (a, tys) =>
	    let 
		fun isNotPempty ty =
		case ty of
		  Base (_, ltokens) => 
		    (case (hd ltokens) of 
		     (Pempty, _) => false
		     | _ => true)
		 | _ => true 

		val nonPemptyTys = List.filter isNotPempty tys
	     in
		if length nonPemptyTys = 0 
		  then genEmptyBase a (getCoverage ty)
		else if length nonPemptyTys = 1 then Poption(a, (hd nonPemptyTys))
	   	else ty
(*
		if length tys = length nonPemptyTys then ty (* no Pempty in this list *)
		else (* some Pemptys exist *)
		  if length nonPemptyTys = 0 (* all Pempty *)
		  then genEmptyBase a (getCoverage ty)
		  else
		    let
(*
		      val _ = (print "Before:\n"; printTy (measure ty))
*)
		      val unionCoverage = sumCoverage nonPemptyTys
		      val newTy = Poption (a, Punion((mkTyAux unionCoverage), nonPemptyTys))
(*
		      val _ = (print "After:\n"; printTy (measure newTy))
*)
		    in newTy
		  end
*)
	     end	
	| _ => ty

(* post constraint rules, these require the cmap to be filled  and the 
data labeled *)

(* a unique Base type becomes a constant type *)
(* Notice that the unique constraint has not been taken away from the LabelMap *)
and uniqueness_to_const cmos ty =
case ty of
  Base({coverage, label=SOME id, ...}, tokens) => 
  if length tokens>0 andalso refineableBase (#1 (hd tokens)) then
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
         	| SOME(Pint(x, _)) => 
			(
				newcmos, 
				RefinedBase((mkTyAux1(coverage, id)), 
				IntConst(x), tokens)
			)
		| SOME(Pfloat(x)) => 
			(
				newcmos, 
				RefinedBase((mkTyAux1(coverage, id)), 
				FloatConst(x), tokens)
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
		| SOME(Pemail(x)) => 
			(
				newcmos, 
				RefinedBase((mkTyAux1(coverage, id)), 
				StringConst(x), tokens)
			)
		| SOME(Pmac(x)) => 
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
		| SOME(Ptext(x)) => 
			(
				newcmos, 
				RefinedBase((mkTyAux1(coverage, id)), 
				StringConst(x), tokens)
			)
		| SOME(Other(x)) => 
			(
				newcmos, 
				RefinedBase((mkTyAux1(coverage, id)), 
				StringConst(str(x)), tokens)
			)
(*
		| SOME(Pempty) => 
			(
				newcmos, 
				RefinedBase((mkTyAux1(coverage, id)), 
				StringConst(""), tokens)
			)
*)
       		| _ => (cmos, ty)
      	end
    | NONE => (cmos, ty)
    )
   else (cmos, ty)
| _ => (cmos, ty)

(* convert a sum into a switched type if a switch constraint is defined *)
(* a sum can only be determined by a base value that is the son of any of the sum's
   ancestors, for a start, we just look at the sum's siblings in a tuple *) 
and sum_to_switch cmos ty =
case ty of 
  Pstruct(aux, tylist) =>
  let	
	(* function to test if a base value with a specific id exists in a ty list*) 
	(* only int base or refined base as well as enum can be considered *)
	fun existsbase(tlist, id) = 
		case (tlist) of
			Base(a, (Pint _, l)::ts)::tail => if Atom.same(id, getLabel(a)) then true
							else existsbase(tail, id)
			| RefinedBase(a, (Int _), _)::tail => if Atom.same(id, getLabel(a)) then true
							else existsbase(tail, id)
			| RefinedBase(a, (Enum _), _)::tail => if Atom.same(id, getLabel(a)) then true
							else existsbase(tail, id)
			| ty::tail => if (Atom.same(getLabel(getAuxInfo(ty)), id)) then false
					else existsbase(tail, id)
			| nil => false
	
	(* test if a sum is a switched sum depending on some other id, test only the first n elements of tylist*)
	fun is_switch(cmos, id, n) = 
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
					length(#1 (hd mappings))=1 andalso existsbase(List.take(tylist, n), hd ids)
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
			([SOME tok1], SOME(Pint(i, _)))::tail => 
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
		    fun reorder refTyList =
			let fun isDefault (re, switchedTy) =
				case re of 
				  StringConst "*" => true
				  | _ => false
			    fun isNotDefault (re, switchedTy) = not (isDefault (re, switchedTy))
			    val defaultpairs = List.filter isDefault refTyList
			    val others = List.filter isNotDefault refTyList
			in (others@defaultpairs)
			end
		    val refine_ty_list = reorder(gen_ref_ty_list (mappings, tlist, 1))
		in
		    if (length refine_ty_list = length tlist) 
		    then (Switch (aux, id, refine_ty_list))
		    else ty
		end
	   | _ => ty

 	fun containsPempty tylist =
	  case tylist of
		nil => false
		| Poption(_, _)::_ => true
		| Base(_, ((Pempty, _)::_))::_ => true
		| ty::tail => containsPempty tail

	fun rewrite_switch (cmos, tlist) =
	    case tlist of 
		h::rest => 
		(
		case h of 
		  Punion(a, sumlist) => 
		    if (containsPempty sumlist) then
		    	let val (newcmos, rest')=rewrite_switch(cmos, rest)
		       	in (newcmos, h::rest')
			end
		    else
			  let 
			    val (c, newcmos)  = is_switch(cmos, some(#label a), (length tylist)-(length tlist)) 
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

and to_dependent_array_len cmos ty =
  let fun findLenDeps id =
	case LabelMap.find(cmos, id) of
	SOME consts =>
	  (
	   let fun f consts =
		case consts of
	 	nil => nil
		| (Eq x) :: rest => (Eq x) :: (f rest)
		| _ :: rest => f rest
	   in f consts
	   end
	  )
	| NONE => nil

  (* function to find a list of int types and the first RArray that 
	hasn't had a lens defined *)
  fun findIntTypesAndArray tylist intlist = 
    case tylist of
	(t as RefinedBase (a, Int x, tlist))::l => findIntTypesAndArray l (t::intlist)
	| (t as (Base (a, (Pint _, _)::_))) :: l => findIntTypesAndArray l (t::intlist)
	| ((t as RArray (a, sep, term, body, None, lens)) :: _ ) => (intlist, SOME t)
	| _::l => findIntTypesAndArray l intlist
	| nil => (nil, NONE)

  fun in_list tys id =
	case tys of
	  t::tys => 
	   if Atom.toString id = Atom.toString (getLabel(getAuxInfo t)) then true
	   else in_list tys id
	| nil => false

  fun replace_array tys (RArray (a, sep, term, body, lenop, lens))=
	(
	case tys of
	  (RArray(a, _, _, _, _, _))::tys => (RArray (a, sep, term, body, lenop, lens))::tys
	| ty::tys => ty::replace_array tys (RArray (a, sep, term, body, lenop, lens))
	| nil => nil
	)
    | replace_array tys _ = tys
  fun get_one_const consts tys =
	case consts of
	(c as Eq ([(id, _)], _))::consts => if in_list tys id then SOME c
					    else get_one_const consts tys
	| _::consts => get_one_const consts tys
	| nil => NONE
  in
  case ty of
    Pstruct(aux, tylist) =>
	let val (intTypes, arrayop) = findIntTypesAndArray tylist nil in
	case arrayop of
	  SOME (RArray (a, sep, term, body, None, lens)) =>
		let val lensConsts = findLenDeps (some(#label a))
		in
		  case get_one_const lensConsts intTypes of
		    SOME (Eq ([(id, _)], _)) =>
		      let val newarray = RArray(a, sep, term, body, SOME (LabelRef id), lens) in
		       (cmos, Pstruct(aux, replace_array tylist newarray))
		      end
		  | _ => (cmos, ty)
		end
	| _ => (cmos, ty)
	end
  | _ => (cmos, ty)
  end
 
(*convert an enum constraint to a Enum refined type or a range constraint to 
	a range refined type *)
and enum_range_to_refine cmos ty = 
  case ty of                  
    Base({coverage, label=SOME(id), ...}, b) => 
      if length b>0 andalso enumerableBase (#1 (hd b)) then
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
				 else if (allStringConsts refs) then
				  let
		                  (*funtion to sort the all string const refined types by 
				    the length of the strings from longest to shortest, 
				    this is so as to attemp the longer and more specific
		      		    strings first*)
		    			fun shorter (re1, re2) =
					  case (re1, re2) of
					  (StringConst x, StringConst y) => (size x < size y)
					  | _ => raise TyMismatch
					val sorted_res = ListMergeSort.sort shorter refs
				  in
					RefinedBase(mkTyAux1(coverage, id), Enum sorted_res, b)
				  end
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
     else (cmos, ty)
  | _ => (cmos, ty)


(* check if a given ty is a blob we use Ptext to stand for blob *)
(* NOTE: if might the case that we should check by variance first before checking
  components of a ty 
and isBlob ty = 
  case ty of
	  (* a base type of blob is a blob *)
	  Base(a, ((Ptext _, l)::ltl)) => true
	  (* if a struct contains only blobs and then it is also a blob *)
	| Pstruct (a, tys) => foldl myand true (map isBlob tys)
	| Punion (a, tys) => 
	  let val allblobs =  foldl myand true (map isBlob tys)
	  in
	  (* if all branches of a union are blob then it is also a blob *)
	  allblobs orelse (check_by_variance ty) end
	| Parray (a, {tokens = t, lengths = lens, first = f, body = b, last = l}) =>
		if (isBlob f andalso isBlob b andalso isBlob l) then true
		else check_by_variance ty
	| RefinedBase (a,  Enum _, _) => check_by_variance ty
	| Switch (a, id, retys) => 
	  let val allblobs =  foldl myand true (map (fn (re, t) => isBlob t) retys) in
	  allblobs  orelse check_by_variance ty end
	| RArray (a, _, _, t, _, _) =>
		isBlob t orelse check_by_variance ty
	| Poption (a, t) => 
		isBlob t orelse check_by_variance ty
	| _ => false
*******************)
(* check if a ty is a blob by dividing the variance of this ty by the number of
   tokens per record associated with this ty *)

fun mergeAdjPtexts (t1, l1)  (t2, l2) =
	if adjacent (t1, l1) (t2, l2) then
	  case (t1, t2) of
	    (Ptext s1, Ptext s2) => (Ptext (s1 ^ s2), combLoc (l1, l2))
	  | _ => raise InvalidToken
	else ((* print "While merging ty: \n"; printTy ty; *)
		print ((ltokenToString (t1, l1)) ^ "\n" ^ (ltokenToString (t2, l2))); 
		raise InvalidToken)
      (* merge all ptext within a token list into one ptext token *)
(*
      fun mergeAllTokens tl = 
	case tl of
	  nil => raise InvalidToken
	| [(Ptext s, l)] => (Ptext s, l)
	| t::ts => mergeAdjPtexts t (mergeAllTokens ts)
	(* append tl1's element to tl2's elements *)
*)
fun merge_tls (tl1, tl2) =
(*
	let 
	  val _ = print ("TL1 : " ^ (LTokensToString tl1) ^ "\n")
	  val _ = print ("TL2 : " ^ (LTokensToString tl2) ^ "\n")
	in
*)
	  if tl1 = nil then tl2
	  else if tl2 = nil then tl1
	  else
	   (* we take one element from tl1 and check against
		every element in tl2 in order and find the
		element in tl2 whose location immediately precedes
		the element in tl1 and stick these two element together,
		if not found, put this element in tl2 *)
	   let fun appendto backl (t, outputl) =
		case List.find 
		  (fn t' => adjacent t t') backl of
		  SOME ltoken => outputl @ [mergeAdjPtexts t ltoken]
		| NONE =>  (outputl @ [t])
	       fun prepend frontl (t, outputl) =
		case List.find 
		  (fn t' => adjacent t' t) frontl of
		  SOME ltoken => outputl @ [mergeAdjPtexts ltoken t]
		| NONE =>  (outputl @ [t])
	   in	
	   if length tl2 >= length tl1 (* tl1 is subset of tl2*)
	   then	foldl (appendto tl1) [] tl2
	   else foldl (prepend tl2) [] tl1 
	   end
(*
	end
*)

(* merge all the tokens belonging to a ty to one single token list *)
(* invarants are that the token list are ordered by their line no and
   and the two corresponding tokens in lists are adjacent to each other*)
fun mergeTokens ty =
  let 
      fun mysort tl = 
	    let fun gt ((t1, l1), (t2, l2)) = (compLocation (l1, l2) = GREATER)
	    in
	    ListMergeSort.sort gt tl
	    end
      fun tos (t, l) = (Ptext (tokenToOrigString t), l)
      fun collapse (tl : LToken list) sep =
	let fun col_helper tl cur_tok newtl =
	  case tl of
	    nil => 
		(
		  case cur_tok of
		    NONE => newtl
		  | SOME t => newtl @ [t]
		)
	  | t :: tl => 
		(
		  case cur_tok of
		    NONE => col_helper tl (SOME t) newtl
		  | SOME (ct as (Ptext s, loc)) => 
			if adjacent ct t then 
			  col_helper tl (SOME (mergeAdjPtexts ct t)) newtl
			else 
			  let val newloc = {lineNo = (#lineNo loc), beginloc = (#beginloc loc),
				endloc = (#endloc loc) + (size sep), recNo = (#recNo loc)} 
		  	      val ct_sep = (Ptext (s ^ sep), newloc) 
		          in
			     col_helper (t :: tl) NONE (newtl @ [ct_sep])
			  end
		 | _ => raise InvalidToken
		)
	in col_helper tl NONE nil
	end

(*
      fun collapse (tl : LToken list)  sep lengths =
	case lengths of
	  nil => nil
	| (l, recNo)::lengths => 
	  if l < 1 then collapse tl sep lengths
	  else 
	  let val subtl = List.take (tl, l) 
	      val _ = print ("Len = " ^ (Int.toString l) ^ " RecNo = " ^ (Int.toString recNo) ^ "\n")
	      val _ = print ("subtl = " ^ (LTokensToString subtl))
	      val (Ptext s, loc) = mergeAllTokens subtl
	      val ptokens = collapse (List.drop (tl, l)) sep lengths
	  in
	      case ptokens of
		nil => [(Ptext s, loc)]
	      | _ => 
		let val newloc = {lineNo = (#lineNo loc), beginloc = (#beginloc loc),
			endloc = (#endloc loc) + (size sep), recNo = (#recNo loc)} in
		  (Ptext (s ^ sep), newloc) :: ptokens
		end
	  end
*)
  in
    let val final_tl = 
      case ty of
	   Base (a, l) => 
		if isEmpty ty then nil else map tos (mysort l)
        |  TBD (a, _, l) => raise TyMismatch
        |  Bottom (a, _, l) => raise TyMismatch
        |  Pstruct (a, tys) => 
		(* assume none of the tys are empty ty *)
		let val ltl_list = map mergeTokens tys
		in
		  foldl merge_tls nil ltl_list
		end
        |  Punion (a, tys)       => 
		let val nonEmptyTys = List.filter (fn ty => not (isEmpty ty)) tys
		    val tls = map mergeTokens nonEmptyTys
		in mysort (List.concat tls) end
        |  Parray (a, {tokens=t, lengths=len, first=f, body=b, last=l}) => 
		raise TyMismatch
		(*
		merge_tls ((merge_tls ((mergeTokens f), (mergeTokens b))), (mergeTokens l))
		*)
        |  RefinedBase (aux, re, l) => map tos (mysort l)
        |  Switch (a, id, retys) => 
		let val nonEmptyReTys = List.filter (fn (_, ty) => not (isEmpty ty)) retys
		    val tls = map (fn (_, ty) => mergeTokens ty) nonEmptyReTys
		in mysort (List.concat tls)
		end
        |  RArray (a,sep,term,body,len,lengths) => 
		let val tl = mergeTokens body
		val sepstr = case sep of 
			SOME (IntConst a) => LargeInt.toString a
			| SOME (FloatConst (a, b)) => a ^ "." ^ b
			| SOME (StringConst s) => s
			| _ => ""
		in
		   ((*print "Collapsing array:\n";
		   printTy ty; *)
		   collapse tl sepstr)
		end 
        |  Poption (a, body)  => mergeTokens body
     in
       final_tl
(*
       if getCoverage ty = length final_tl then final_tl
       else (print ("Coverage : " ^ (Int.toString (getCoverage ty)) ^ "Lengths : " ^ 
		(Int.toString (length final_tl)) ^ "\n" ^
		(LTokensToString final_tl));  raise Unexpected)
*)
     end
  end

(*
fun mergeTokens records ty =
	let fun mysort locs = 
	    let fun gt (l1, l2) = (compLocation (l1, l2) = GREATER)
	    in
	    ListMergeSort.sort gt locs
	    end
	fun unique_lines better (tl: LToken list) =
	  let val map = 
	    foldl (fn ((token, loc), intmap) =>
	      let val _ = if (#lineNo loc) < 0 then
		print (ltokenToString (token, loc))
		else ()
	      in
		case (IntMap.find (intmap, (#lineNo loc))) of
		SOME exist_loc => 
		  if better loc exist_loc then 
			let val (newmap, _) = IntMap.remove (intmap, (#lineNo loc))
			in IntMap.insert (newmap, (#lineNo loc), loc)
			end
		  else intmap
		| None => IntMap.insert (intmap, (#lineNo loc), loc)
	       end
	    ) IntMap.empty tl
	  in IntMap.listItems map
	  end
	fun get_left_most_locs ty = 
	  let fun b l1 l2 = (compLocation (l1, l2) = LESS)
	  in
	  case ty of
    	      Base (a, l)           => unique_lines b l
            | TBD (a, _, cl)            => unique_lines b (map hd cl)
            | Bottom (a, _, cl)         => unique_lines b (map hd cl)
            | Pstruct (a, tys)      => 
    		if length tys = 0 then raise TyMismatch
    		else get_left_most_locs (hd tys)
            | Punion (a, tys)       => mysort (List.concat (map get_left_most_locs tys))
            | Parray (a, {tokens=t, lengths=len, first=f, body=b, last=l}) => 
    		get_left_most_locs f
            | RefinedBase (aux, re, l) => unique_lines b l
            | Switch (a, id, retys) => mysort (List.concat 
    			(map (fn (r, t) => get_left_most_locs t) retys))
            | RArray (a,sep,term,body,len,lengths) => get_left_most_locs body
            | Poption (a, body) => raise NoOption (* doesn't allow Poption for now *)
	  end

	fun get_right_most_locs ty = 
	  let fun b l1 l2 = (compLocation (l1, l2) = GREATER)
	  in
	  case ty of
    	    Base (a, l)           => unique_lines b l
            | TBD (a, _, l)            => unique_lines b (map List.last l)
            | Bottom (a, _, l)         => unique_lines b (map List.last l)
            | Pstruct (a, tys)      => 
    		if length tys = 0 then raise TyMismatch
    		else get_right_most_locs (List.last tys)
            | Punion (a, tys)       => mysort(List.concat (map get_right_most_locs tys))
            | Parray (a, {tokens=t, lengths=len, first=f, body=b, last=l}) => 
    		get_right_most_locs l
            | RefinedBase (aux, re, l) => unique_lines b l
            | Switch (a, id, retys) => mysort(List.concat 
    			(map (fn (r, t) => get_right_most_locs t) retys))
            | RArray (a,sep,term,body,len,lengths) => get_right_most_locs body
            | Poption (a, body) => raise NoOption (* doesn't allow Poption for now *)
	  end
	val _ = print "getleft\n"
	val blocs = get_left_most_locs ty
	val _ = print "getright\n"
	val elocs = get_right_most_locs ty
	fun myzip bls els = 
	      case (bls, els) of
		(nil, nil) => nil
	      | (bloc::bls, eloc::els) =>
		  if (#lineNo bloc) = (#lineNo eloc) andalso 
			(#beginloc bloc) <= (#endloc eloc) then 
		    (combLoc (bloc, eloc)) :: (myzip bls els)
	          else raise LocMismatch
	      | _ => raise LocMismatch
	val _ = print "zip\n"
	val newlocs = myzip blocs elocs
	fun formPtext records (loc : location) =
		let 
		    val _ = print ("Getting from line #" ^ (Int.toString (#lineNo loc)) ^ "\n")
		    val r = List.nth (records, (#lineNo loc)) 
		    val substr = String.substring (r, #beginloc loc, 
					(#endloc loc) - (#beginloc loc) + 1)
		in
		   (Ptext substr, loc)
		end
	val _ = print "map\n"
	in
	  map (formPtext records) newlocs
	end
*)

fun isBlob ty =
  case ty of
    Base _ => false
  | RefinedBase _ => false
  | Poption _ => false
  (* | Pstruct _ => false *)
  | _ =>
	let val avgNumTokensPerRec = (Real.fromInt (getNumTokens ty)) / 
			(Real.fromInt (getCoverage ty)) 
	    val tyc = toReal (getTypeComp ty)
	    val adc = toReal (getAtomicComp ty) 
	    val var = variance ty
	    val ratio = var / avgNumTokensPerRec 
	    val ratio1 = tyc / adc
(*
	    val _ = print "For Ty .....\n"
	    val _ = printTy ty
	    val _ = print ("AvgNumTokensPerRec = " ^ (Real.toString avgNumTokensPerRec) ^ "\n")
	    val _ = print ("Variance = " ^ Real.toString var ^ "\n")
*)
	    val _ = print ((getLabelString (getAuxInfo ty)) ^ ":\t")
	    val _ = print ("Ratio = " ^ Real.toString ratio ^ "\t")
	    val _ = print ("Comp Ratio = " ^ Real.toString ratio1 ^ "\n")
	in
	  ratio > 1.0 andalso (ratio + ratio1 > 4.0)
	end
(* TODO: augment this function to search for more patterns by merging
  all tokens in the ty and then do string matching *)
fun getStoppingPatt ty =
  case ty of
    RefinedBase (a, StringME regex, _) => (NONE, SOME regex)
  | RefinedBase (a, IntConst i, _) => (SOME (LargeInt.toString i), NONE)
  | RefinedBase (a, FloatConst (i, d), _) => (SOME (i ^ "." ^ d), NONE)
  | RefinedBase (a, StringConst s, _) => (SOME s, NONE)
  | _ => (NONE, NONE)

fun containString ltokens str =
	case ltokens of
	  nil => false
	| (t, loc)::rest => 
	  (
	    case t of
	      Ptext s => if String.isSubstring str s then true
			 else containString rest str
	    | _ => raise InvalidToken
	  )
	
fun containPatt ltokens patt = true (* assume true for now as we don't have regex yet *)


(* update the current ty to a possible ty if the sibling contains legit stopping pattern *)	
(* NOTE: we don't go inside array for now *)	
fun updateWithBlobs s_opt ty =
(*
  let fun f tys =
    case tys of
	nil => nil
      | [ty] => [mkBlob NONE ty]
      | ty::(sib::x) => (mkBlob (SOME sib) ty):: (f (sib::x))
  in
*)
  let fun f tys s_opt = 
	case tys of
	  nil => nil
	| ty::tys => 
	    let val newty = updateWithBlobs s_opt ty 
	    in
		case newty of
		  RefinedBase (a, Blob _, _) => newty::(f tys NONE)
		| _ => newty::(f tys (SOME newty))
	    end
  fun mergeAdjBlobs curBlob tys newtys =
   let fun mergeBlobs b1 b2 = 
	case (b1, b2) of
	(RefinedBase (a1, Blob _, tl1), RefinedBase (a2, Blob x, tl2)) =>
	  RefinedBase (a1, Blob x, merge_tls (tl1, tl2))
	| _ => raise TyMismatch
   in
     case tys of
	(b as RefinedBase (a, Blob _, tl)) :: tys => 
		(
		case curBlob of
		  SOME cb =>
		    let val newb = mergeBlobs cb b in
		      mergeAdjBlobs (SOME newb) tys newtys
		    end
		| NONE => mergeAdjBlobs (SOME b) tys newtys
		)
	| t :: tys => 
		(
		case curBlob of 
		  SOME cb => mergeAdjBlobs NONE tys (newtys@[cb, t])
		| _ => mergeAdjBlobs NONE tys (newtys @ [t])
		)
	| nil => 
		(
		case curBlob of 
		  SOME cb => (newtys@[cb])
		| _ => newtys
		)
    end
  fun isBlobTy ty =
	case ty of
	  RefinedBase (_, Blob _, _) => true
	| _ => false
  in
  case ty of
	  Pstruct(a, tys) => 
	   let val newtys = List.rev (f (List.rev tys) s_opt)
(*
	       val _ = print "**** BEGIN ****************\n"
	       val _ = map printTy newtys
	       val _ = print "**** END ****************\n"
*)
	       in mkBlob s_opt (Pstruct (a, mergeAdjBlobs NONE newtys nil))
	   end
   	| Punion(a, tys) =>
	    let val newtys = map (updateWithBlobs s_opt) tys 
	        val blobtys = List.filter isBlobTy newtys
		val newblob = List.foldl (fn (blob, l) =>
				case l of
				  nil => [blob]
				| [oldblob] => [mergeTy (oldblob, blob)]
				| _ => raise Unexpected) nil blobtys
		val nonblobtys = List.filter (fn x => not (isBlobTy x)) newtys
	    in
		if (List.length nonblobtys) = 0 andalso (List.length newblob) = 1 
		then (hd newblob)
		else 
	          mkBlob s_opt (Punion (a, (nonblobtys @ newblob)))
	    end
	| RArray (a, sep, term, body, fixed, lengths) =>
	   (
	    case (sep, term) of
	    (SOME s, SOME t) => 
		if refine_equal (s, t) then
		  (* use a dummy refinedbase type as righthand side sibling *)
		  let val sib_opt = SOME (RefinedBase (a, s, nil)) in
	    	    mkBlob s_opt (RArray(a, sep, term, updateWithBlobs sib_opt body, fixed, lengths))
		  end
		else mkBlob s_opt (RArray (a, sep, term, body, fixed, lengths))
	    | _ => mkBlob s_opt (RArray (a, sep, term, body, fixed, lengths))
	   )
	| Switch(aux, id, retys) =>
	    let val newretys = map (fn (re, t) => (re, updateWithBlobs s_opt t)) retys 
		val blobretys = List.filter (fn (re, t) => isBlobTy t) retys in
	    if (length retys = length blobretys andalso length retys > 0) then (* all blobs *)
	      let val newblob = List.foldl (fn ((r, blob), l) =>
			case l of
			  nil => [blob]
			| [oldblob] => [mergeTy (oldblob, blob)]
			| _ => raise Unexpected) nil blobretys in
	        hd newblob
	      end
	    else
	      mkBlob s_opt (Switch(aux, id, newretys))
	    end
	| Poption (aux, ty) => mkBlob s_opt (Poption(aux, updateWithBlobs s_opt ty))
	| _ => ty
  end	

and mkBlob sibling_opt ty = 
  if isBlob (measure ty) then
    let val ltokens = mergeTokens ty 
	(* val _ = printTy ty  *)
	(* val _ = print (LTokensToString ltokens)  *)
    in 
    case sibling_opt of
	  NONE => 
		let val newty = RefinedBase(getAuxInfo ty, Blob(NONE, NONE), ltokens)
		    val _ = print "******* FOUND BLOB ABOVE ******\n"
		in newty 
(*
		if score newty < score ty then newty 
		else updateWithBlobs sibling_opt ty
*)
		end
	| SOME sibty =>
	  (
		let
		  (* val _ = print "Getting stopping patt\n" *)
		  val pair = getStoppingPatt sibty in
		case pair of 
		  (SOME str, NONE) => 
		    if containString ltokens str then ty
		    else 
			let val newty = RefinedBase(getAuxInfo ty, Blob pair, ltokens)
		            val _ = print "******* FOUND BLOB ABOVE ******\n"
			in newty
	(*
			if score newty < score ty then newty 
			else updateWithBlobs sibling_opt ty
	*)
			end
		| (NONE, SOME str ) => 
		    if containPatt ltokens str then ty
		    else 
			let val newty = RefinedBase(getAuxInfo ty, Blob pair , ltokens)
		            val _ = print "******* FOUND BLOB ABOVE ******\n"
			in newty
	(*
			if score newty < score ty then newty 
			else updateWithBlobs sibling_opt ty
	*)
			end
		| _ => ty
		end
	)
    end
  else ty

	
(* the actual reduce function can either take a SOME(const_map) or
NONE.  It will use the constraints that it can apply. *)
fun reduce phase ty = 
let
  val phase_one_rules : pre_reduction_rule list = 
		[ 	
			remove_degenerate_list,
			unnest_tuples,
			unnest_sums,
			prefix_postfix_sums,
			remove_nils,
		  	unused_branches,
(*
			extract_table_header,
*)
			union_to_optional,
			struct_to_array,
			find_neg_num,
			to_float,
			refine_array
		]
  val phase_two_rules : post_reduction_rule list =
		[ 
		  uniqueness_to_const, 
		  adjacent_consts,
		  enum_range_to_refine,
		  sum_to_switch,
		  to_dependent_array_len
		]
  val phase_three_rules : pre_reduction_rule list = 
		[ 	
			remove_degenerate_list,
			unnest_tuples,
			unnest_sums,
			prefix_postfix_sums,
			remove_nils,
		  	unused_branches,
			union_to_optional
(*
			, extract_table_header
*)
		]

  (* generate the list of rules *)
  val cmap = case phase of
	2 => Constraint.constrain' ty
	| _ => LabelMap.empty
(* Print the constraints  
  val _ = printConstMap cmap
*)

  (* returns a new cmap after reducing all the tys in the list and a new tylist *)
  fun mymap f phase cmap tylist newlist =
	case tylist of 
		ty::tail => let 
				val(cmap', ty') = f phase cmap ty
			    in	mymap f phase cmap' tail (newlist@[ty'])
			    end
		| nil => (cmap, newlist)

  (*reduce a ty and returns the new constraint map and the new ty *)
  (* phase = 0: pre_constraint; phase = 1: post_constraint *)
  fun reduce' phase cmap ty =
    let 
      	(* go bottom up, calling reduce' on children values first *)
      	val (newcmap, reduced_ty) = 
			case ty of
			  Pstruct (a, tylist) => let 
				val (cmap', tylist') = mymap reduce' phase cmap tylist nil
				in (cmap', Pstruct (a, tylist'))
				end
			| Punion (a, tylist) => let
				val (cmap', tylist') = mymap reduce' phase cmap tylist nil
				in (cmap', (measure (Punion(a, tylist'))))
				end
			| Parray (a, {tokens, lengths, first, body, last}) => 
				let
				val (cmap1, firstty) = reduce' phase cmap first
				val (cmap2, bodyty) = reduce' phase cmap1 body 
				val (cmap3, lastty) = reduce' phase cmap2 last 
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
                                val (cmap', tylist') = mymap reduce' phase cmap tylist nil
                                in (cmap', Switch (a, id, ListPair.zip(refs, tylist')))
                                end
                        | RArray (a, sep, term, body, len, lengths) => 
                                let
                                val (cmap', body') = reduce' phase cmap body
                                in
                                (cmap', RArray (a, sep, term, body', len, lengths))
                                end
			| Poption (a, body) => 
                                let
                                val (cmap', body') = reduce' phase cmap body
                                in
                                (cmap', Poption(a, body'))
				end

	  fun iterate cmap ty = 
	  let
	    (* calculate the current cost *)
	    (*
	    val _ = (print ("Old Ty: \n"); printTy (measure ty))
	    *)
	    val cur_cost = score ty
	    (* apply each rule to the ty *)
	    val cmap_ty_pairs = case phase of
			1 => map(fn x => (cmap, x ty)) phase_one_rules
		|	2 => map (fn x => x cmap ty) phase_two_rules
		|	3 => map(fn x => (cmap, x ty)) phase_three_rules
		| 	_ => (print "Wrong phase!\n"; raise TyMismatch)
	    (* find the costs for each one *)
	    val costs = map (fn (m, t)=> score t) cmap_ty_pairs 
	    val pairs = ListPair.zip(cmap_ty_pairs,costs)
	    (* we do greedy descent for now *)
	    fun min((a, b),(c, d)) = 
		if b < d then (a, b) else (c, d)
	    (* find the minimum cost out of the ones found *)
	    val ((newcmap, newTy), lowCost) = foldr min ((cmap, ty), cur_cost) pairs
	  in
	  	(* as long as the cost keeps going down, keep iterating *)
	  	if lowCost < cur_cost then 
		((*print "Old Ty:\n"; printTy (measure ty); 
		 print "New Ty:\n"; printTy (measure newTy);*) 
		 iterate newcmap newTy)
	  	else (newcmap, newTy) 
	  end
    in
 	(iterate newcmap reduced_ty) 
    end
(*val cbefore = cost cmap ty *)
  val (cmap', ty') = reduce' phase cmap ty 
(*  val cafter = cost cmap' ty'*)
(*  val _ = print ("Before:" ^ (Int.toString cbefore) ^ " After:" ^ (Int.toString cafter) ^ "\n") *)
in
  sortUnionBranches ty'
end 

end
