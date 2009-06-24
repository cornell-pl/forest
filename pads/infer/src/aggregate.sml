structure Aggregate = 
struct
  open Parse
  exception MergeFailed

  (* this table is actually implemented as a set, the length of the element in this set (the
     int list) can grow *)
  structure IntListSet = SplaySetFn(struct
		type ord_key = int list
		val compare = fn (list1, list2) =>
			if length list1 <> length list2 then raise Unexpected
			else List.collate Int.compare (list1, list2)
  end)

  (* len is the length of the rows which may grow *)
  type OptsTable = {header: Id list, rows: IntListSet.set}
  fun initTable() : OptsTable = {header = nil, rows = IntListSet.empty}
  fun printTable (table: OptsTable) =
	(
	List.app (fn id => print ((Atom.toString id) ^ "\t")) (#header table);
	print "\n";
	IntListSet.app (fn l => 
		(List.app (fn x => print ((Int.toString x) ^ "\t")) l;
		 print "\n")) (#rows table)
	)

  fun printTableSize (table: OptsTable) =
    let val numcols = length (#header table)
	val numrows = IntListSet.numItems (#rows table)
    in 
        print ("OptsTable: numcols = " ^ (Int.toString numcols) ^ " numrows = " ^
		(Int.toString numrows) ^ "\n")
    end

  datatype Aggr =
	  BaseA of Token
	| SyncA of Refined option
	| TupleA of Aggr list
	| UnionA of Aggr list  (* one per branch *)
	| SwitchA of (Refined * Aggr) list  (* one per branch plus additional new branches *)
	| ArrayA of (Aggr * Aggr * Aggr) (* elements, seps, terms *)
	| OptionA of Aggr
	| Opt of (Id * Aggr)	   (* the id will become the label of the new Option node *)
	| Ln of (Id * string list) (* learn node combines the strings to be learned with
					the good stuff in an aggregate of the orig node
				      the id will become the label of the new Option node *)

  (* this function adds a list of branching decisions to the OptsTable, 
     assuming the decisions are already sorted by the Id of the Opts that parsed them *)
  fun addToTable (table: OptsTable) (pairs: (Id * int) list) : OptsTable = 
    let 
(*
	val _ = print "before\n"
	val _ = printTable table
	val _ = print "binlist to add:\n"
	val _ = List.app (fn (id, x) => print (Atom.toString id ^ ": " ^ Int.toString x ^ " ")) pairs
	val _ = print "\n"
*)
	val hdrs = (#header table)
	fun correlate hdrs pairs =
	  case (hdrs, pairs) of
	    (nil, nil) => nil
	  | (nil, pairs) => pairs
	  | (id::hdrs, nil) => 
		(id, 0)::(correlate hdrs nil)
	  | (id::hdrs, (id', i)::pairs') =>
	      if Atom.same (id, id') then (id', i)::(correlate hdrs pairs')	
	      else (id, 0)::(correlate hdrs pairs)
	val newpairs = correlate hdrs pairs
	val newlen = length newpairs
        val (expandedRows, new_hdrs) = 
          if newlen > (length hdrs) then 
	    let val tail = List.drop (newpairs, length hdrs)
	        val (tail_hdrs, tail_ints) = ListPair.unzip tail
	        (* we need to append the reversed the tail of the new list to 
		every existing member of the set *)
	        val rev_tail_ints = map (fn x => if x = 1 then 0 else 1) tail_ints
	    in (IntListSet.map (fn l => l@rev_tail_ints) (#rows table), hdrs@tail_hdrs)
	    end 
          else ((#rows table), hdrs)
	val newRows = IntListSet.add (expandedRows, (#2 (ListPair.unzip newpairs)))
(*	val _ = print "after \n" *)
    in
	{header = new_hdrs, rows = newRows}
    end	

  (* function to transport the OptsTable to a map indexed by ids *)
  fun transpose (table: OptsTable) : (int list) LabelMap.map =
	let val list_rows = IntListSet.listItems (#rows table)
	    val len = length (#header table)
	    fun get_col index = map (fn l => List.nth (l, index)) list_rows
	    fun f ids index map =
		case ids of
		  id::ids => 
			let val map' = LabelMap.insert (map, id, get_col index)
			in f ids (index+1) map'
			end
		| nil => map
	in f (#header table) 0 LabelMap.empty
	end

  (* find the correlation of two ids in the map: 0 means neg correlated, 1 means pos correlated,
     -1 means not correlated *)	
  fun getCorrelation map id1 id2 =
	let val list1 = case LabelMap.find (map, id1) of
			NONE => raise Unexpected
			| SOME l => l
	    val list2 = case LabelMap.find (map, id2) of
			NONE => raise Unexpected
			| SOME l => l
	    fun f (l1, l2) col =
	      case (l1, l2) of
	        (i1::l1, i2::l2) =>  
		  if col = 0 then
			if i1 + i2 = 1 then f (l1, l2) col
			else (~1)
		  else if col = 1 then
			if i1 + i2 = 1 then (~1)
			else f (l1, l2) col
		  else if i1 = i2 then f (l1, l2) 1
		       else f (l1, l2) 0
	     | (nil, nil) => col
	     | _ => raise Unexpected
	in f (list1, list2) (~1)
	end handle Unexpected => (~1)

(* 
 fun getBitVectorString map id =
   case LabelMap.find (map, id) of
	SOME l => SOME (String.concat (map Int.toString l))
	| NONE => NONE
*)


  fun aggrToString prefix r =
    case r of
    BaseA t => prefix ^ "BaseA (" ^ tokenTyToString t ^ ")\n"
  | SyncA (SOME re) => prefix ^ "SyncA (" ^ refinedToString re ^ ")\n"
  | SyncA NONE => prefix ^ "SyncA (None)\n"
  | Opt (id, agg) =>
	prefix ^ "Opt (" ^ Atom.toString id ^ ") {\n" ^
	  aggrToString (prefix ^ "    ") agg
	^ prefix ^ "}\n"
  | TupleA aggs => 
	let val ss = map (aggrToString (prefix ^ "    ")) aggs 
	in
	   prefix ^ "TupleA {\n" ^
	   (String.concat ss) ^ 
	   prefix ^ "}\n"
	end
  | UnionA branches =>
	let val ss = map (aggrToString (prefix ^ "    ")) branches
	in
	   prefix ^ "UnionA {\n" ^
	   (String.concat ss) ^ 
	   prefix ^ "}\n"
	end
  | SwitchA branches =>
	let val ss = map (fn (re, a) => prefix ^ "    " ^ (refinedToString re) ^ " =>\n" ^
			(aggrToString (prefix ^ "    ") a)) branches
	in
	   prefix ^ "SwitchA {\n" ^
	   (String.concat ss) ^ 
	   prefix ^ "}\n"
	end
  | ArrayA (elemA, sepA, termA) =>
      let 
	val elem_string = prefix ^ "ELEM:\n" ^ (aggrToString (prefix ^ "    ") elemA) 
	val sep_string = prefix ^ "SEP:\n" ^ (aggrToString (prefix ^ "    ") sepA) 
	val term_string = prefix ^ "TERM:\n" ^ (aggrToString (prefix ^ "    ") termA) 
      in 
	prefix ^ "Array {\n" ^
	elem_string ^
	sep_string ^
	term_string ^
	prefix ^ "}\n"
      end
  | OptionA agg =>
	prefix ^ "OptionA {\n" ^
	  aggrToString (prefix ^ "    ") agg
	^ prefix ^ "}\n"
  | Ln (id, strings) =>
	prefix ^ "Learn (" ^ Atom.toString id ^ ") {\n" ^
	(String.concat (map (fn s => prefix ^ "    \"" ^ s ^ "\"\n") strings)) ^
	prefix ^ "}\n"

  fun equal_aggr (ag1, ag2) =
     case (ag1, ag2) of
       (BaseA t1, BaseA t2) => compToken (t1, t2) = EQUAL
     | (SyncA r1, SyncA r2) => refine_equal_op1(r1, r2)
     | (TupleA ags1, TupleA ags2) => ListPair.allEq equal_aggr (ags1, ags2)
     | (UnionA ags1, UnionA ags2) => ListPair.allEq equal_aggr (ags1, ags2)
     | (SwitchA re_ags1, SwitchA re_ags2) =>
		ListPair.allEq (fn ((re1, ag1), (re2, ag2)) => 
				refine_equal(re1, re2) andalso equal_aggr(ag1, ag2))
		(re_ags1, re_ags2)
     | (ArrayA (e1, s1, t1), ArrayA (e2, s2, t2)) =>
	equal_aggr(e1, e2) andalso equal_aggr(s1, s2) andalso equal_aggr(t1, t2)
     | (OptionA ag1, OptionA ag2) => equal_aggr (ag1, ag2)
     | (Opt (_, ag1), Opt (_, ag2)) => equal_aggr (ag1, ag2)
     | (Ln (_, ss1), Ln (_, ss2)) => ListPair.allEq (fn (s1, s2) => s1 = s2) (ss1, ss2)
     | _ => false


  (* function to merge a rep into an aggregate, 
     returns a new aggregate and a list of (id, branch) pairs *)
  fun merge a rep =
    case (a, rep) of
      (BaseA bs, BaseR (GoodB x)) => (BaseA bs, nil)
    | (BaseA bs, BaseR (ErrorB)) => 
		let val id = mkNextTyLabel()
		in (Opt (id, BaseA bs), [(id, 0)])
		end
    | (Opt (id, BaseA bs), BaseR (GoodB x)) => (Opt (id, BaseA bs), [(id, 1)])
    | (Opt (id, BaseA bs), BaseR (ErrorB)) => (Opt (id, BaseA bs), [(id, 0)])

    | (SyncA ss, SyncR (Good s)) => (SyncA ss, nil)
    | (SyncA ss, SyncR (Fail)) => 
		let val id = mkNextTyLabel()
		in (Opt (id, SyncA ss), [(id, 0)])
		end
    | (SyncA ss, SyncR (Partial (s, re))) =>
	(
	case (ss, re) of
	  (SOME (IntConst i), IntConst j) => 
		(SyncA (SOME (Int (LargeInt.min(i, j), LargeInt.max(i, j)))), nil) 
		(* change int const to ranged int *)
	| (SOME (Int (min, max)), IntConst new) => 
		if new < min then (SyncA(SOME (Int (new, max))), nil)
		else (SyncA(SOME (Int(min, new))), nil)
	| (SOME (FloatConst _), FloatConst _) => (* reduce to Pfloat *) 
		(SyncA NONE, nil)
	| (SOME (Enum res), re) => 
		if List.exists (fn re' => refine_equal (re, re')) res then
		  (SyncA ss, nil)
		else (SyncA(SOME (Enum (res@[re]))), nil)
	| (SOME (StringConst s'), StringConst s) =>  (SyncA(SOME (Enum [StringConst s', re])), nil)
	| (NONE, _) => (SyncA NONE, nil)
	| _ => raise MergeFailed
	)
    | (SyncA ss, SyncR (Recovered(r, s, m))) => 
		let val id = mkNextTyLabel()
		in (TupleA [Ln (id, [r]), SyncA ss], [(id, 1)])
		end

    | (Opt (id, SyncA ss), SyncR (Good s)) => (Opt (id, SyncA ss), [(id, 1)])
    | (Opt (id, SyncA ss), SyncR Fail) => (Opt (id, SyncA ss), [(id, 0)])
    | (Opt (id, SyncA ss), SyncR (Partial x)) => 
		(Opt (id, (#1 (merge (SyncA ss) (SyncR (Partial x))))), [(id, 1)])
    | (Opt (id, SyncA ss), SyncR (Recovered(r, s, m))) => 
		let val id1 = mkNextTyLabel()
		in (TupleA [Ln (id1, [r]), Opt(id, SyncA ss)], [(id, 1), (id1, 1)])
		end

    | (TupleA [Ln (id, l), SyncA ss], SyncR (Good s)) => (TupleA [Ln (id, l), SyncA ss], [(id, 0)])
    | (TupleA [Ln (id, l), SyncA ss], SyncR Fail) => 
		(TupleA [Ln (id, l), Opt (mkNextTyLabel(), SyncA ss)], [(id, 0)])
    | (TupleA [Ln (id, l), SyncA ss], SyncR (Partial x)) => 
		(TupleA [Ln (id, l), (#1 (merge (SyncA ss) (SyncR (Partial x))))], [(id, 0)])
    | (TupleA [Ln (id, l), SyncA ss], SyncR (Recovered(r, s, m))) => 
		(TupleA [Ln (id, r::l), SyncA ss], [(id, 1)])

    | (TupleA [Ln (id, l), Opt (id1, SyncA ss)], SyncR (Good s)) => 
		(TupleA [Ln (id, l), Opt (id1, SyncA ss)], [(id, 0), (id1, 1)])
    | (TupleA [Ln (id, l), Opt (id1, SyncA ss)], SyncR Fail) => 
		(TupleA [Ln (id, l), Opt (id1, SyncA ss)], [(id, 0), (id1, 0)])
    | (TupleA [Ln (id, l), Opt (id1, SyncA ss)], SyncR (Partial x)) => 
		(TupleA [Ln (id, l), Opt (id1, (#1 (merge (SyncA ss) (SyncR (Partial x)))))], 
		  [(id, 0), (id1, 1)])
    | (TupleA [Ln (id, l), Opt (id1, SyncA ss)], SyncR (Recovered(r, s, m))) => 
		(TupleA [Ln (id, r::l), Opt(id1, SyncA ss)], [(id, 1), (id1, 1)])

(* The following accumulate the good data on the leaves as well!
      (BaseA bs, BaseR (GoodB x)) => BaseA ((GoodB x)::bs)
    | (BaseA bs, BaseR (ErrorB)) => Opt (BaseA bs)
    | (Opt (BaseA bs), BaseR (GoodB x)) => Opt (BaseA ((GoodB x)::bs))
    | (Opt (BaseA bs), BaseR (ErrorB)) => Opt (BaseA bs)

    | (SyncA ss, SyncR (Good s)) => SyncA ((Good s)::ss)
    | (SyncA ss, SyncR (Fail)) => Opt (SyncA ss)
    | (SyncA ss, SyncR (Recovered(r, s, m))) => TupleA [Ln [r], SyncA ((Good (s, m))::ss)]

    | (Opt (SyncA ss), SyncR (Good s)) => Opt (SyncA ((Good s)::ss))
    | (Opt (SyncA ss), SyncR Fail) => Opt (SyncA ss)
    | (Opt (SyncA ss), SyncR (Recovered(r, s, m))) => TupleA [Ln [r], Opt(SyncA ((Good (s, m))::ss))]

    | (TupleA [Ln l, SyncA ss], SyncR (Good s)) => TupleA [Ln l, SyncA ((Good s)::ss)]
    | (TupleA [Ln l, SyncA ss], SyncR Fail) => TupleA [Ln l, Opt (SyncA ss)]
    | (TupleA [Ln l, SyncA ss], SyncR (Recovered(r, s, m))) => TupleA [Ln (r::l), SyncA ((Good (s, m))::ss)]

    | (TupleA [Ln l, Opt (SyncA ss)], SyncR (Good s)) => TupleA [Ln l, Opt (SyncA ((Good s)::ss))]
    | (TupleA [Ln l, Opt (SyncA ss)], SyncR Fail) => TupleA [Ln l, Opt (SyncA ss)]
    | (TupleA [Ln l, Opt (SyncA ss)], SyncR (Recovered(r, s, m))) => 
		TupleA [Ln (r::l), Opt(SyncA ((Good (s, m))::ss))]
*)

    | (TupleA ags, TupleR reps) =>
	if length ags <> length reps then raise MergeFailed
	else 
	  let val (aggrs, lists) = ListPair.unzip (ListPair.map (fn (a, r) => merge a r) (ags, reps))
	  in (TupleA aggrs, List.concat lists)
	  end
    | (UnionA ags, UnionR (b, rep)) => 
	if b<0 orelse b>=(length ags) then raise MergeFailed
	else 
	  let val a = List.nth (ags, b)
	      val (a', pairs) = merge a rep
	      val prefix = List.take (ags, b)
	      val suffix = List.drop (ags, b+1)
	  in (UnionA (prefix@(a'::suffix)), pairs)
	  end
    | (SwitchA re_ags, SwitchR (re, rep)) =>
	let val newa = 
	  case List.find (fn (r, a) => refine_equal (r, re)) re_ags of
	    SOME (r, Ln (id, ss)) =>  (* this is a previously added recovered branch *)
	     (
		case rep of 	
		  SyncR (Recovered (s, "", StringME("/$/"))) =>
		    let val newpair = (r, Ln (id, ss@[s]))
		        val new_re_ags = map (fn (re, a) => 
				if refine_equal (r, re) then newpair 
			    	else (re, a)) re_ags
		    in
			(SwitchA new_re_ags, [(id, 1)])
		    end
		| _ => raise TyMismatch
	     )
	  | SOME (r, a) =>
		let val (newa, pairs) = merge a rep
		    val newpair = (r, newa)
	            val new_re_ags = map (fn (re, a) => 
				if refine_equal (r, re) then newpair 
			    	else (re, a)) re_ags
		in
		  (SwitchA new_re_ags, pairs)
		end
	  | NONE => 
	     (
		case rep of
		  SyncR (Recovered (s, _, StringME _)) =>
			let val id = mkNextTyLabel()
			in 
			  (SwitchA (re_ags@[(re, Ln (id, [s]))]), [(id, 1)])
			end
		| _ => raise TyMismatch
	     )
	in newa
	end
    | (ArrayA (eleA, sepA, termA), ArrayR(elems, seps, termop)) =>
	let val (eleA', pairs) = foldl (fn (r, (a, l)) => 
				let val (a', l') = (merge a r)
				in (a', l@l')
				end) (eleA, nil) elems
	    val (sepA', pairs) = foldl (fn (r, (a, l)) => 
				let val (a', l') = (merge a r)
				in (a', l@l')
				end) (sepA, pairs) seps
	    val (termA', pairs') = case termop of 
				SOME r => merge termA r
			      | _ => (termA, nil)
	in (ArrayA (eleA', sepA', termA'), pairs@pairs')
	end

    | (OptionA a, OptionR (SOME r)) => 
		let val (newa, pairs) = (merge a r)
		in (OptionA newa, pairs)
		end
    | (OptionA a, OptionR NONE) => (OptionA a, nil)
    | _ => (print (aggrToString "" a); print (repToString "" rep);  raise MergeFailed)

(* function that takes a Ty and generates an initial empty aggregate structure *)
  fun initialize ty = 
    case ty of
	Base (aux, tl) => BaseA (#1 (hd tl))
	| RefinedBase (aux, re, _) => SyncA (SOME re)
	| Pstruct (a, tys) =>
		let val inits = map initialize tys in
		  TupleA inits
		end 
	| Punion (a, tys) => 	
		let val inits = map initialize tys in
		  UnionA inits
		end 
	| Switch (a, id, retys) => 
		let val inits = map (fn (re, ty) => (re, initialize ty)) retys in
		  SwitchA inits
		end
	| RArray (a, sep, term, body, len, lengths) => 
	  	let val eleA = initialize body
		in
		  ArrayA (eleA, SyncA sep, SyncA term)
		end
	| Poption (a, ty) => OptionA (initialize ty)
	| _ => (printTy ty; raise TyMismatch)

  (* function to measure the cost of an aggregate by counting the number of opt and learn nodes *)
  (* NOTE: this may not be adequate as it doesn't take into account the erroneous data being accumulated
     at learn nodes *)
  fun cost a = 
	case a of
	  BaseA _ => 0
	| SyncA _ => 0
	| Opt _ => 1
	| Ln (_, ss) => (foldl (fn (s, len) => (String.size s) + len) 0 ss) 
		(*	/ (Real.fromInt(List.length ss)) *) 
		(* TODO: This may need to be fixed! *)
	| TupleA l => foldl (fn (a, c) => (cost a) + c) 0 l
	| UnionA l => foldl (fn (a, c) => (cost a) + c) 0 l
	| ArrayA (e, s, t) => foldl (fn (a, c) => (cost a) + c) 0 [e, s, t]
	| OptionA a => cost a
	| SwitchA l => foldl (fn ((r, a), c) => (cost a) + c) 0 l

 fun learn lines sibling_opt =
  let 
      val _ = (print "Learning these lines:\n"; List.app (fn s => print (s ^ "\n")) lines) 
      val (ty, _) = Structure.computeStructurefromRecords lines
      val ty = removePempty ty
      val ty = Reduce.reduce 1 ty
      val ty = Reduce.reduce 2 ty
      val ty = Reduce.reduce 3 ty
      val finalty= 
	   if Options.do_blob_finding then
                sortUnionBranches (Reduce.updateWithBlobs sibling_opt ty)
           else sortUnionBranches ty
      val _ = (print ("Learned Ty:\n"); printTy finalty)
  in finalty 
  end

 (* TODO: here we reset all coverage to 0, we may need to keep track of coverage in aggregates 
    when we grow the aggregate *)
 fun updateTy ty aggr =
	case (ty, aggr) of
	  (_, TupleA [a, Ln (id, ss)]) => 
	    if length ss > 0 then
	      let 
		val extra_ty = learn ss NONE
	      in
		Pstruct(mkTyAux 0, [updateTy ty a, Poption(mkTyAux1(0, id) , extra_ty)])
	      end
	    else updateTy ty a
	| (_, TupleA [Ln (id, ss), a]) => 
	    if length ss > 0 then
	      let 
		val sib_ty = updateTy ty a
		val extra_ty = learn ss (SOME sib_ty)
	      in
		Pstruct(mkTyAux 0, [Poption(mkTyAux1 (0, id), extra_ty), sib_ty])
	      end
	    else updateTy ty a
	| (_, Opt (id, a)) => Poption (mkTyAux1 (0, id), updateTy ty a)
	| (Base (a, tl), BaseA _) => ty
	| (RefinedBase (a, re, tl), SyncA (SOME newre)) => RefinedBase (a, newre, tl)
	| (RefinedBase (a, re, tl), SyncA NONE) => Base (a, tl) (* resetting to Base type *)
	| (Pstruct(a, tys), TupleA aggrs) => 
		if (length tys = length aggrs) then
		  Pstruct(a, ListPair.map (fn (t, ag) => updateTy t ag) (tys, aggrs))
		else raise TyMismatch
 	| (Punion(a, tys), UnionA aggrs) =>
		if (length tys = length aggrs) then
		  Punion(a, ListPair.map (fn (t, ag) => updateTy t ag) (tys, aggrs))
		else raise TyMismatch
	| (Switch(a, id, retys) , SwitchA re_ags) => (* re_ags maybe longer than retys *)
		let val retys' =
			ListPair.map (fn ((re, ty), (re1, ag)) =>
			  if refine_equal (re, re1) then 
				(re, updateTy ty ag) 
			  else raise TyMismatch) (retys, re_ags)
		    val extra_re_ags = List.drop (re_ags, length retys)
		    val extras = map (fn (re, ag) =>
					case ag of
					  Ln (_, ss) => (re, learn ss NONE)
					| _ => raise TyMismatch) extra_re_ags
		in Switch (a, id, (retys' @ extras))
		end
	| (RArray (a, sep, term, body, len, lengths), ArrayA (e, s, t)) =>
	(* TODO: we don't have a way of modifying the len and lengths yet *)
		let
		  val newbody = updateTy body e
		  val sep = case (sep, s) of
			(SOME sep, SyncA (SOME re)) => SOME re
			| (NONE, SyncA NONE) => NONE
			| _ => raise TyMismatch
		  val term = case (term, t) of
			(SOME term, SyncA (SOME re)) => SOME re
			| (NONE, SyncA NONE) => NONE
			| _ => raise TyMismatch
	    	in RArray(a, sep, term, newbody, len, lengths)
		end
	| (Poption (a, ty), OptionA ag) => Poption (a, updateTy ty ag)
	| _ => (print "updateTy failed!\n"; printTy ty; 
		print (aggrToString "" aggr); 
		raise TyMismatch)
		

(* merge all adj options within a struct, with the help of a dependency map *)
fun merge_adj_options dep_map ty =
  case ty of
    Pstruct (a, tylist) => 
     let
	fun f l =
	case l of
	  (Poption (a1, t1)::Poption (a2, t2)::l) =>
	  if getCorrelation dep_map (getLabel a1) (getLabel a2) = 1 then
	  let val cov = getCoverage t1
	      val aux = mkTyAux cov
	      val newstruct = Pstruct (aux, [t1, t2])
	      val newl = Poption(a1, Reduce.unnest_tuples newstruct) :: l
	  in f newl
	  end
	  else Poption(a1, t1)::(f (Poption(a2, t2)::l))
       | (t::l) => t:: (f l)
       | nil => nil
       val tylist1 = map (merge_adj_options dep_map) tylist
     in
	Pstruct(a, f tylist1)
     end
  | Punion (a, tylist) => Punion (a, map (merge_adj_options dep_map) tylist)
  | Base b => Base b
  | RefinedBase b => RefinedBase b
  | Switch (a, id, pairs) =>  
          let
	  val newpairs = map (fn (re, ty) => (re, merge_adj_options dep_map ty)) pairs
          in Switch (a, id, newpairs)
          end
  | RArray (a, sep, term, body, len, lengths) => 
          let
          val body' = merge_adj_options dep_map body
          in
            RArray (a, sep, term, body', len, lengths)
          end
  | Poption (a, body) => Poption(a, merge_adj_options dep_map body)
  | _ => (print ("Bad Ty:\n"); printTy ty; raise TyMismatch)

 
(* consider situation of Pstruct [opt1, opt2, opt3, opt4, opt5, ...]
opt1, opt3, and opt5 are postively correlated, opt2 and opt4 are also
positively correlated, but the two groups are negatively correlated, then
generate a union of these two groups *)
fun alt_options_to_unions dep_map ty =
  case ty of
    Pstruct (a, tylist) =>
	let fun f l =
	  case l of
	    Poption (a1, t1) :: Poption(a2, t2) :: l =>
		let val label1 = getLabel(a1)
		    val label2 = getLabel(a2)
		in 
		  if getCorrelation dep_map label1 label2 = 0 then
		    f (Punion(a1, [Pstruct (a1, [t1]), Pstruct(a2, [t2])]) :: l)
		  else Poption(a1, t1) :: (f (Poption(a2, t2) :: l))
		end
	  | Punion (a, [t1, t2]) :: Poption (a3, t3) :: l => 
		let val label1 = getLabel(getAuxInfo t1)
		    val label2 = getLabel(getAuxInfo t2)
		    val label3 = getLabel(getAuxInfo t3)
		    val col_1_2 = getCorrelation dep_map label1 label2 
		    val col_1_3 = getCorrelation dep_map label1 label3 
		in
		    if col_1_2 = 0 andalso col_1_3 = 1 then
			case t1 of
			  Pstruct(a1, tys) => 
			    f (Punion(a, [Pstruct(a1, tys@[t3]), t2]) :: l)
			| _ =>
			    f (Punion(a, [Pstruct(a3, [t1, t3]), t2]) :: l)
		    else if col_1_2 = 0 andalso col_1_3 = 0 then
			case t2 of
			  Pstruct(a2, tys) => 
			    f (Punion(a, [t1, Pstruct(a2, tys@[t3])]) :: l)
		        | _ => 
			    f (Punion(a, [t1, Pstruct(a3, [t2, t3])]) :: l)
		   else Punion(a, [t1, t2]) :: (f (Poption (a3, t3) :: l))
		end
	  | t :: l => t :: (f l)
	  | nil => nil
	  val newtylist = map (alt_options_to_unions dep_map) tylist
	in Pstruct (a, f newtylist)
	end		  
  | Punion (a, tylist) => Punion (a, map (alt_options_to_unions dep_map) tylist)
  | Base b => Base b
  | RefinedBase b => RefinedBase b
  | Switch (a, id, pairs) =>  
          let
	  val newpairs = map (fn (re, ty) => (re, alt_options_to_unions dep_map ty)) pairs
          in Switch (a, id, newpairs)
          end
  | RArray (a, sep, term, body, len, lengths) => 
          let
          val body' = alt_options_to_unions dep_map body
          in
            RArray (a, sep, term, body', len, lengths)
          end
  | Poption (a, body) => Poption(a, alt_options_to_unions dep_map body)
  | _ => raise TyMismatch


end
    
