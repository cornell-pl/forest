structure Aggregate = 
struct
  open Parse
  exception MergeFailed

  datatype Aggr =
	  BaseA of BaseData list
	| SyncA of SyncData list
	| TupleA of Aggr list
	| UnionA of Aggr list  (* one per branch *)
	| ArrayA of (Aggr * Aggr * Aggr) (* elements, seps, terms *)
	| OptionA of Aggr
	| Opt of Aggr
	| Ln of (string list * Aggr) (* learn node combines the strings to be learned with
					the good stuff in an aggregate of the orig node *)

  (* function to merge a rep into an aggregate *)
  fun merge a rep =
    case (a, rep) of
      (BaseA bs, BaseR (GoodB x)) => BaseA ((GoodB x)::bs)
    | (BaseA bs, BaseR (ErrorB)) => Opt (BaseA bs)
    | (Opt (BaseA bs), BaseR (GoodB x)) => Opt (BaseA ((GoodB x)::bs))
    | (Opt (BaseA bs), BaseR (ErrorB)) => Opt (BaseA bs)

    | (SyncA ss, SyncR (Good s)) => SyncA ((Good s)::ss)
    | (SyncA ss, SyncR (Fail)) => Opt (SyncA ss)
    | (SyncA ss, SyncR (Recovered(r, m))) => Ln ([r], SyncA ((Good m)::ss))

    | (Opt (SyncA ss), SyncR (Good s)) => Opt (SyncA ((Good s)::ss))
    | (Opt (SyncA ss), SyncR Fail) => Opt (SyncA ss)
    | (Opt (SyncA ss), SyncR (Recovered(r, m))) => Ln ([r], Opt( SyncA ((Good m)::ss)))

    | (Ln (l, SyncA ss), SyncR (Good s)) => Ln (l, SyncA ((Good s)::ss))
    | (Ln (l, SyncA ss), SyncR Fail) => Ln (l, Opt (SyncA ss))
    | (Ln (l, SyncA ss), SyncR (Recovered(r, m))) => Ln (r::l, SyncA ((Good m)::ss))

    | (Ln (l, Opt (SyncA ss)), SyncR (Good s)) => Ln (l, Opt (SyncA ((Good s)::ss)))
    | (Ln (l, Opt (SyncA ss)), SyncR Fail) => Ln (l, Opt (SyncA ss))
    | (Ln (l, Opt (SyncA ss)), SyncR (Recovered(r, m))) => Ln (r::l, Opt(SyncA ((Good m)::ss)))

    | (TupleA ags, TupleR reps) =>
	if length ags <> length reps then raise MergeFailed
	else TupleA (ListPair.map (fn (a, r) => merge a r) (ags, reps))
    | (UnionA ags, UnionR (b, rep)) => 
	if b<0 orelse b>=(length ags) then raise MergeFailed
	else 
	  let val a = List.nth (ags, b)
	      val a' = merge a rep
	      val prefix = List.take (ags, b)
	      val suffix = List.drop (ags, b+1)
	  in UnionA (prefix@(a'::suffix))	
	  end
    | (ArrayA (eleA, sepA, termA), ArrayR(elems, seps, termop)) =>
	let val eleA' = foldl (fn (r, a) => (merge a r)) eleA elems
	    val sepA' = foldl (fn (r, a) => (merge a r)) sepA seps
	    val termA' = case termop of 
				SOME r => merge termA r
			      | _ => termA
	in ArrayA (eleA', sepA', termA')
	end

    | (OptionA a, OptionR (SOME r)) => OptionA (merge a r)
    | (OptionA a, OptionR NONE) => OptionA a
    | _ => raise MergeFailed

  (* function that takes a Ty and generates an initial empty aggregate structure *)
  fun initialize ty = 
    case ty of
	Base _ => BaseA nil
	| RefinedBase _ => SyncA nil
	| Pstruct (a, tys) =>
		let val inits = map initialize tys in
		  TupleA inits
		end 
	| Punion (a, tys) => 	
		let val inits = map initialize tys in
		  UnionA inits
		end 
	| Switch (a, id, retys) => 
		let val inits = map (fn (re, ty) => initialize ty) retys in
		  UnionA inits
		end
	| RArray (a, sep, term, body, len, lengths) => 
	  	let val eleA = initialize body
		in
		  ArrayA (eleA, SyncA nil, SyncA nil)
		end
	| Poption (a, ty) => OptionA (initialize ty)
	| _ => raise TyMismatch	

  (* function to measure the cost of an aggregate by counting the number of opt and learn nodes *)
  (* NOTE: this may not be adequate as it doesn't take into account the erroneous data being accumulated
     at learn nodes *)
  fun cost a = 
	case a of
	  BaseA _ => 0
	| SyncA _ => 0
	| Opt _ => 1
	| Ln (_, a) => 1 + cost a
	| TupleA l => foldl (fn (a, c) => (cost a) + c) 0 l
	| UnionA l => foldl (fn (a, c) => (cost a) + c) 0 l
	| ArrayA (e, s, t) => foldl (fn (a, c) => (cost a) + c) 0 [e, s, t]
	| OptionA a => cost a

fun aggrToString prefix r =
  case r of
    BaseA l => prefix ^ "BaseA\n"
  | SyncA l => prefix ^ "SyncA\n"
  | Opt agg =>
	prefix ^ "Opt {\n" ^
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
  | Ln (strings, a) =>
	prefix ^ "Learn {\n" ^
	(String.concat (map (fn s => prefix ^ "\t\"" ^ s ^ "\"\n") strings)) ^
	aggrToString (prefix ^ "    ") a
	^ prefix ^ "}\n"

end

	
    

      
