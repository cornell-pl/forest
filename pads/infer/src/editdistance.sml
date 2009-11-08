structure Editdistance =
struct
  open Types
  open Common

  type TyForrest = Ty list

  fun forrestcompare (tys1 : Ty list, tys2 : Ty list) : order =
    let fun comp(l1, l2) =
	case (l1, l2) of
	  (t1::l1, t2::l2) => 
	  let val myorder = tycompare (t1, t2) 
	  in
	    if myorder = EQUAL then comp(l1, l2)
	    else myorder
	  end
	| (nil, nil) => EQUAL
	| _ => raise TyMismatch
    in
			    
	if length tys1 < length tys2 then LESS
	else if length tys1 > length tys2 then GREATER
	else comp(tys1, tys2)
    end

  fun forrestPairCompare ((f1, f2), (f3, f4)) =
	let val order1 = forrestcompare (f1, f3)
	in
	  if order1 = EQUAL then forrestcompare (f2, f4)
	  else order1
	end
	
  structure EditDistMap = SplayMapFn(
	struct type ord_key = TyForrest * TyForrest
	       val compare = forrestPairCompare
	end)

  val edMap = ref (EditDistMap.empty: int EditDistMap.map)

  fun rmRoot ty =
    case ty of
 	Base _ => nil
	| TBD _ => nil
	| Bottom _ => nil
	| RefinedBase _ => nil
	| Pstruct (a, tys) => tys
	| Punion(a, tys) => tys
	| Parray(a, {tokens=ts, lengths=lens, first=f, body=b, last=l}) => [f, b, l]
	| Switch(a,id, retys) => #2 (ListPair.unzip retys)
	| RArray(a, sep, term, body, len, lengths) => [body]
	| Poption (a, body) => [body]
	
  (* we assume unit cost for all edits for now! *)
  fun editCost (ty1_opt, ty2_opt) =
	case (ty1_opt, ty2_opt) of
	  (NONE, NONE) => 0
	| (SOME t1, SOME t2) =>
		if ty_equal(1, t1, t2) then 0
		else 1
	| _ => 1

  fun editDist (f1: TyForrest, f2:TyForrest) : int =
   case (f1, f2) of
     (nil, nil) => 0
   | (f1, nil) => 
	(
	case EditDistMap.find (!edMap, (f1, nil)) of
	  SOME x => x
	| NONE =>
	  (
	   case List.rev f1 of
		t::l => 
		  let val c = editCost(SOME t, NONE)
		      val tailforrest = rmRoot t
		      val newdist = editDist ((List.rev l) @ tailforrest, nil) + c
		      val _ = edMap := EditDistMap.insert (!edMap, (f1, nil), newdist)
		      val _ = edMap := EditDistMap.insert (!edMap, (nil, f1), newdist)
		  in
		      newdist
		  end
	       | _ => raise TyMismatch
	  )
	)
   | (nil, f2) => 
	(
	case EditDistMap.find (!edMap, (nil, f2)) of
	  SOME x => x
	| NONE =>
	  (
	   case List.rev f2 of
		t::l => 
		  let val c = editCost(NONE, SOME t)
		      val tailforrest = rmRoot t
		      val newdist = editDist (nil, (List.rev l) @ tailforrest) + c
		      val _ = edMap := EditDistMap.insert (!edMap, (nil, f2), newdist)
		      val _ = edMap := EditDistMap.insert (!edMap, (f2, nil), newdist)
		  in
		      newdist
		  end
	       | _ => raise TyMismatch
	  )
	)
  | _ =>
	if forrestcompare (f1, f2) = EQUAL then 0
	else
	(
	case EditDistMap.find (!edMap, (f1, f2)) of
	  SOME x => x
	| NONE =>
	  (
	  case (List.rev f1, List.rev f2) of
	    (t1::l1, t2::l2) =>
		let val tailf1 = rmRoot t1
		    val tailf2 = rmRoot t2
		    val dist1 = editDist((List.rev l1)@ tailf1, f2) + editCost(SOME t1, NONE)
		    val dist2 = editDist(f1, (List.rev l2)@ tailf2) + editCost(NONE, SOME t2)
		    val dist3 = editDist(tailf1, tailf2) + editDist(List.rev l1, List.rev l2)
				+ editCost(SOME t1, SOME t2)
		    fun less x y = x < y
		    val newdist = min less [dist1, dist2, dist3]
		    val _ = edMap := EditDistMap.insert(!edMap, (f1, f2), newdist)
		    val _ = edMap := EditDistMap.insert(!edMap, (f2, f1), newdist)
		in
		  newdist
		end
	  | _ => raise TyMismatch
	  )
	)
		
end	
