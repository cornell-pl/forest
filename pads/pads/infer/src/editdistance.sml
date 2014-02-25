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
	| (l1, nil) => GREATER
	| (nil, l2) => LESS
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

  fun stringPairCompare ((s1, s2), (s3, s4)) =
	let val order1 = String.compare (s1, s3)
	in if order1 = EQUAL then String.compare (s2, s4)
	   else order1
	end

  fun stringListCompare (l1, l2) =
	case (l1, l2) of
	  (s1::l1, s2::l2) => 
	    let val myorder = String.compare (s1, s2)
	    in
		if myorder = EQUAL then stringListCompare (l1, l2)
	  	else myorder
	    end
	| (nil, nil) => EQUAL
	| (l1, nil) => GREATER
	| (nil, l2) => LESS

  fun stringListPairCompare ((l1, l2), (l3, l4)) =
	let val order1 = stringListCompare (l1, l3)
	in
	  if order1 = EQUAL then stringListCompare (l2, l4)
	  else order1
	end
   
(*	
  structure EditDistMap = SplayMapFn(
	struct type ord_key = TyForrest * TyForrest
	       val compare = forrestPairCompare
	end)
*)
  structure EditDistMap = SplayMapFn(
	struct type ord_key = string * string 
	       val compare = stringPairCompare
	end)

  structure ForrestDistMap = SplayMapFn(
	struct type ord_key = string list * string list
	       val compare = stringListPairCompare
	end)

  val edMap = ref (EditDistMap.empty: int EditDistMap.map)
  
  fun getKeyRoots ty set =
    let fun add_list (t, s) = 
	let val l = getLabelString (getAuxInfo t)
 	in StringSet.add (s, l)
	end
	fun g (t, s) = getKeyRoots t s
    in
      case ty of
 	Base _ => set
	| TBD _ => set
	| Bottom _ => set
	| RefinedBase _ => set
	| Pstruct (a, tys) => 
	   let val set' = foldl g set tys 
	   in
	     if length tys > 1 then foldl add_list set' (tl tys)
	     else set'
	   end
	| Punion(a, tys) => 
	   let val set' = foldl g set tys 
	   in
	     if length tys > 1 then foldl add_list set' (tl tys)
	     else set'
	   end
	| Parray(a, {tokens=ts, lengths=lens, first=f, body=b, last=l}) => 
	   let val set' = foldl g set [f, b, l] 
	   in
	     foldl add_list set' [b, l]
	   end
	| Switch(a,id, retys) => 
	  let val subtys = #2 (ListPair.unzip retys)
	      val set' = foldl g set subtys
	  in
	    if length retys > 1 then foldl add_list set' (tl subtys)
	    else set'
	  end
	| RArray(a, sep, term, body, len, lengths) => g (body, set)
	| Poption (a, body) => g (body, set)
    end

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

  fun numNodes ty =
    case ty of
 	Base _ => 1
	| TBD _ => 1
	| Bottom _ => 1
	| RefinedBase _ => 1
	| Pstruct (a, tys) => foldl op+ 1 (map numNodes tys)
	| Punion(a, tys) => foldl op+ 1 (map numNodes tys)
	| Parray(a, {tokens=ts, lengths=lens, first=f, body=b, last=l}) => foldl op+ 1 (map numNodes [f, b, l])
	| Switch(a,id, retys) => foldl op+ 1 (map numNodes (#2 (ListPair.unzip retys)))
	| RArray(a, sep, term, body, len, lengths) => 1 + numNodes body
	| Poption (a, body) => 1 + numNodes body

  fun tyname_equal (ty1, ty2) = 
   case (ty1, ty2) of
     (Base(_, tl1), Base(_, tl2)) => ltoken_ty_equal(hd tl1, hd tl2)
   | (RefinedBase (_, r1, _), RefinedBase(_, r2, _)) => refine_equal(r1, r2)	
   | (TBD _, TBD _) => true
   | (Bottom _, Bottom _) => true
   | (Punion _, Punion _) => true
   | (Pstruct _, Pstruct _) => true
   | (Parray _, Parray _) => true
   | (Switch (_, id1, _), Switch(_, id2, _)) => true
   | (RArray (_, sepop1, termop1, ty1, len1, _),
      RArray (_, sepop2, termop2, ty2, len2, _)) =>
	refine_equal_op1(sepop1, sepop2) andalso
	refine_equal_op1(termop1, termop2) andalso
        refine_equal_op1(len1, len2)
   | (Poption _, Poption _) => true
   | _ => false  

  (* we assume unit cost for all edits for now! *)
  fun editCost (ty1_opt, ty2_opt) =
	case (ty1_opt, ty2_opt) of
	  (NONE, NONE) => 0
	| (SOME t1, SOME t2) =>
		if tyname_equal(t1, t2) then 0
		else 1
	| _ => 1

(****************************** OLD implementation *****************
  fun editDist (f1: TyForrest, f2:TyForrest) : int =
   let val _ = print ("map size = " ^ Int.toString (EditDistMap.numItems (!edMap)) ^ "\n")
   in
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
***********************************)

  val keyRoots1 = ref StringSet.empty
  val keyRoots2 = ref StringSet.empty

  fun getLabelStr ty = getLabelString (getAuxInfo ty)

  fun treeEditDist (ty1, ty2) = 
    let val l1 = (getLabelStr ty1)
        val l2 = (getLabelStr ty2)
	val fmap = ref (ForrestDistMap.empty : int ForrestDistMap.map)
    in
	case EditDistMap.find(!edMap, (l1, l2)) of
	  SOME d => d
	| NONE => 
		let 
		  val des1 = rmRoot ty1
		  val des2 = rmRoot ty2
		  val d = editDist(fmap, des1, des2) + editCost(SOME ty1, SOME ty2)
		  val _ = edMap := EditDistMap.insert(!edMap, (l1, l2), d)
		in d
		end
    end

  and editDist (fmap, f1: TyForrest, f2:TyForrest) : int =
   case (f1, f2) of
     (nil, nil) => 0
   | (f1, nil) => 
	let val labels = map getLabelStr f1
	in
	case ForrestDistMap.find (!fmap, (labels, nil)) of
	  SOME d => d
	| NONE =>
	  (
	   case List.rev f1 of
		t::l => 
		  let val c = editCost(SOME t, NONE)
		      val tailforrest = rmRoot t
		      val newdist = editDist (fmap, (List.rev l) @ tailforrest, nil) + c
		      val _ = fmap := ForrestDistMap.insert (!fmap, (labels, nil), newdist)
		  in
		      newdist
		  end
	       | _ => raise TyMismatch
	  )
	end
   | (nil, f2) => 
	let val labels = map getLabelStr f2
	in
	case ForrestDistMap.find (!fmap, (nil, labels)) of
	  SOME d => d
	| NONE =>
	  (
	   case List.rev f2 of
		t::l => 
		  let val c = editCost(NONE, SOME t)
		      val tailforrest = rmRoot t
		      val newdist = editDist (fmap, nil, (List.rev l) @ tailforrest) + c
		      val _ = fmap := ForrestDistMap.insert (!fmap, (nil, labels), newdist)
		  in
		      newdist
		  end
	       | _ => raise TyMismatch
	  )
	end
  | _ =>
	let val labels1 = map getLabelStr f1
	    val labels2 = map getLabelStr f2
	in
	case ForrestDistMap.find (!fmap, (labels1, labels2)) of
	  SOME d => d
	| NONE =>
	  (
	  case (List.rev f1, List.rev f2) of
	    (t1::l1, t2::l2) =>
		let val tailf1 = rmRoot t1
		    val tailf2 = rmRoot t2
		    val dist1 = editDist(fmap, (List.rev l1)@ tailf1, f2) + editCost(SOME t1, NONE)
		    val dist2 = editDist(fmap, f1, (List.rev l2)@ tailf2) + editCost(NONE, SOME t2)
		    val dist3 = 
			if StringSet.member(!keyRoots1, getLabelStr t1) andalso
			   StringSet.member(!keyRoots2, getLabelStr t2) 
			then
			  editDist(fmap, (List.rev l1) @ tailf1, (List.rev l2) @ tailf2)
				+ editCost(SOME t1, SOME t2)
			else
			  editDist(fmap, List.rev l1, List.rev l2) + treeEditDist(t1, t2)
		    fun less x y = x < y
		    val newdist = min less [dist1, dist2, dist3]
		    val _ = fmap := ForrestDistMap.insert (!fmap, (labels1, labels2), newdist)
		in
		  newdist
		end
	  | _ => raise TyMismatch
	  ) 
	end

   fun treeEditDistance (ty1, ty2) =
	let val _ = keyRoots1 := getKeyRoots ty1 StringSet.empty
	    val _ = keyRoots2 := getKeyRoots ty2 StringSet.empty
	    val _ = keyRoots1 := StringSet.add (!keyRoots1, getLabelStr ty1)
	    val _ = keyRoots2 := StringSet.add (!keyRoots2, getLabelStr ty2)
	    (* val _ = print ("number of roots: " ^ Int.toString (StringSet.numItems (!keyRoots1)) ^ " " ^
			Int.toString (StringSet.numItems (!keyRoots2)) ^ "\n") *)

	in
	   treeEditDist(ty1, ty2)
	end
	
end	
