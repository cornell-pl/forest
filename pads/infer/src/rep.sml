structure Rep =
struct

open Types

datatype BaseData =
  GoodB of Token
| ErrorB

datatype SyncData =
  Good of Refined
| Recovered of (string * Refined)  (* recovered string, actual matched Refined pair *)
| Fail

type metric_type = int * int * int (* (number of error nodes, recovered chars, total chars parsed ) *)

fun add_metric (e1, r1, t1) (e2, r2, t2) = (e1 + e2, r1 + r2, t1 + t2) 

fun better_metric (e1, r1, t1) (e2, r2, t2) =
	case (t1, t2) of
	  (0, 0) => e1 < e2
	| (0, _) => false
	| (_, 0) => true
	| _ => 
	  let val n1 = (Real.fromInt r1) / (Real.fromInt (r1 + t1))
	      val n2 = (Real.fromInt r2) / (Real.fromInt (r2 + t2))
	  in if n1 < n2 then true
	     else if n1 > n2 then false
	     else e1 < e2
	  end

fun metricToString (e, r, parsed) = "(" ^ Int.toString e ^ ", " ^ Int.toString r ^ ", " 
			^ Int.toString parsed ^ ")"

fun comp_refines (l1, l2) =
  case (l1, l2) of
    (nil, nil) => EQUAL
  | (nil, _) => LESS
  | (_, nil) => GREATER
  | (re1::l1, re2::l2) => 
	let val res = refined_comp (re1, re2) 
	in if res = EQUAL then comp_refines (l1, l2)
	   else res
	end

and refined_comp (r1, r2) =
  case (r1, r2) of
    (StringME s1, StringME s2) => String.compare (s1, s2)
  | (Int(min1, max1), Int(min2, max2)) => 
	if (min1 < min2) then LESS
	else if (min1 > min2) then GREATER
	else if max1 < max2 then LESS
	else if max1 > max2 then GREATER
	else EQUAL
  | (IntConst a, IntConst b) => LargeInt.compare (a, b)
  | (FloatConst (a1, b1), FloatConst (a2, b2)) => String.compare ((a1 ^ b1), (a2 ^ b2))
  | (StringConst s1, StringConst s2) => String.compare (s1, s2)
  | (Enum l1, Enum l2) => comp_refines (l1, l2)
  | (LabelRef id1, LabelRef id2) => Atom.compare (id1, id2)
  | (Blob _, Blob _) => EQUAL
  | (StringME _, _) => LESS
  | (Int _, StringME _) => GREATER
  | (Int _, _) => LESS
  | (IntConst _, StringME _) => GREATER
  | (IntConst _, Int _) => GREATER
  | (IntConst _, _) => LESS
  | (FloatConst _, StringME _) => GREATER
  | (FloatConst _, Int _) => GREATER
  | (FloatConst _, IntConst _) => GREATER
  | (FloatConst _, _) => LESS
  | (StringConst _, StringME _) => GREATER
  | (StringConst _, Int _) => GREATER
  | (StringConst _, IntConst _) => GREATER
  | (StringConst _, FloatConst _) => GREATER
  | (StringConst _, _) => LESS
  | (Enum _, StringME _) => GREATER
  | (Enum _, Int _) => GREATER
  | (Enum _, IntConst _) => GREATER
  | (Enum _, FloatConst _) => GREATER
  | (Enum _, StringConst _) => GREATER
  | (Enum _, _) => LESS
  | (LabelRef _, StringME _) => GREATER
  | (LabelRef _, Int _) => GREATER
  | (LabelRef _, IntConst _) => GREATER
  | (LabelRef _, FloatConst _) => GREATER
  | (LabelRef _, StringConst _) => GREATER
  | (LabelRef _, Enum _) => GREATER
  | (LabelRef _, _) => LESS
  | (Blob _, _) => GREATER


fun sync_comp (syn1, syn2) = 
  case (syn1, syn2) of
    (Good s1, Good s2) => refined_comp (s1, s2)
  | (Recovered (s1, t1), Recovered (s2, t2)) => 
	(
	case String.compare(s1, s2) of
	  GREATER => GREATER
	| LESS => LESS
	| _ => refined_comp (t1, t2) 
	)
  | (Fail, Fail) => EQUAL
  | (Good _, _) => LESS
  | (Recovered _, Good _) => GREATER
  | (Recovered _, _) => LESS
  | (Fail, _) => GREATER

datatype Rep = 
  BaseR of BaseData
| SyncR of SyncData
| TupleR of Rep list
| UnionR of int * Rep  (* int is branch number *)
| ArrayR of (Rep list * Rep list * Rep option)
| OptionR of (Rep option) 
| SwitchR of Refined * Rep

fun compTokenDetail (t1, t2) =
  let val result = compToken (t1, t2) 
  in 
    if result <> EQUAL then result
    else 
        case (t1,t2) of
           (Ptime i1, Ptime i2)           => String.compare (i1, i2)
        |  (Pdate i1, Pdate i2)           => String.compare (i1, i2)
        |  (Purl i1, Purl i2)             => String.compare (i1, i2)
        |  (Ppath i1, Ppath i2)           => String.compare (i1, i2)
        |  (Pip i1, Pip i2)               => String.compare (i1, i2)
        |  (Phostname i1, Phostname i2)   => String.compare (i1, i2)
        |  (Pemail i1, Pemail i2)         => String.compare (i1, i2)
        |  (Pmac i1, Pmac i2)             => String.compare (i1, i2)
        |  (PbXML (f1,s1), PbXML (f2,s2)) => String.compare(f1,f2)
        |  (PeXML (f1,s1), PeXML (f2,s2)) => String.compare(f1,f2)
        |  (Pint (i1, _) , Pint (i2, _)) => LargeInt.compare(i1, i2)
        |  (Pfloat (d1, f1) , Pfloat (d2, f2)) => String.compare(d1 ^ f1, d2 ^ f2)
        |  (Pstring s1, Pstring s2)       => String.compare (s1, s2)
        |  (Pwhite s1, Pwhite s2)         => String.compare (s1, s2)
        |  (Pgroup g1, Pgroup g2)         => compTokenDetail(#1(#left g1), (#1(#left g2)))
        |  (Other c1, Other c2)           => Char.compare (c1, c2)
        |  (Pempty, Pempty)               => EQUAL
        |  (Ptext a, Ptext b)           => String.compare(a, b)
        |  (Error, Error)                 => EQUAL
	| _ => EQUAL
    end

fun base_comp (b1, b2) =
  case (b1, b2) of
    (GoodB t1, GoodB t2) => compTokenDetail (t1, t2)
  | (ErrorB, ErrorB ) => EQUAL
  | (GoodB _, ErrorB) => LESS
  | (ErrorB, GoodB _ ) => GREATER
 
fun comparelist (rs1, rs2) : order =
  case (rs1, rs2) of
    (nil, nil) => EQUAL
  | (nil, _) => LESS
  | (_, nil) => GREATER
  | (r1::rs1, r2::rs2) => 
  	let val result = compare (r1, r2) in
	  if result = EQUAL then comparelist (rs1, rs2)
	  else result
	end
and compare (r1 : Rep, r2 : Rep) : order = 
 case (r1, r2) of
   (BaseR b1, BaseR b2) => base_comp (b1, b2)
 | (SyncR s1, SyncR s2) => sync_comp (s1, s2) 
 | (TupleR rs1, TupleR rs2) => 
	if length rs1 < length rs2 then LESS
	else if length rs1 > length rs2 then GREATER
	else comparelist (rs1, rs2) 
 | (UnionR (b1, r1), UnionR(b2, r2)) => compare (r1, r2)
 | (ArrayR (rs1, ss1, s1), ArrayR (rs2, ss2, s2)) =>
	if (length rs1) < (length rs2) then LESS
	else if (length rs1) > (length rs2) then GREATER
	else let val result_bodys = comparelist (rs1, rs2)
	     in
		if result_bodys = EQUAL then
		  let val result_seps = comparelist (ss1, ss2) in
		    if result_seps = EQUAL then
		      case (s1, s2) of
		        (NONE, SOME _) => LESS
		      | (SOME _, NONE) => GREATER
		      | (SOME r1, SOME r2) =>  compare (r1, r2)
		      | _ => EQUAL
		    else result_seps
		  end
		else result_bodys
	      end
 | (OptionR r1, OptionR r2) =>
	(
	case (r1, r2) of
	(NONE, NONE) => EQUAL
	| (NONE, _) => LESS
	| (SOME _, NONE) => GREATER
	| (SOME x, SOME y) => compare (x, y)
	)
 | (SwitchR (re1, rep1), SwitchR (re2, rep2)) =>
	let val re_comp = refined_comp (re1, re2)
	in if re_comp = EQUAL then compare (rep1, rep2)
	   else re_comp
	end
 | (BaseR _, _) => LESS
 | (SyncR _ , BaseR _) => GREATER
 | (SyncR _, _ ) => LESS
 | (TupleR _, BaseR _) => GREATER
 | (TupleR _, SyncR _) => GREATER
 | (TupleR _, _) => LESS
 | (UnionR _, BaseR _) => GREATER
 | (UnionR _, SyncR _ ) => GREATER
 | (UnionR _, TupleR _) => GREATER
 | (UnionR _, _) => LESS
 | (ArrayR _, BaseR _) => GREATER
 | (ArrayR _, SyncR _ ) => GREATER
 | (ArrayR _, TupleR _) => GREATER
 | (ArrayR _, UnionR _) => GREATER
 | (ArrayR _, _) => LESS
 | (OptionR _, BaseR _) => GREATER
 | (OptionR _, SyncR _ ) => GREATER
 | (OptionR _, TupleR _) => GREATER
 | (OptionR _, UnionR _) => GREATER
 | (OptionR _, ArrayR _) => GREATER
 | (OptionR _, _) => LESS
 | (SwitchR _, _) => GREATER

(* function to display a rep *)
fun repToString prefix r =
  case r of
    BaseR (GoodB t) => prefix ^ "GoodB(" ^ tokenToString t ^ ")\n"
  | BaseR (ErrorB) => prefix ^ "ErrorB\n"
  | SyncR (Good s) => prefix ^ "Good(" ^ refinedToString s ^ ")\n"
  | SyncR (Recovered (s1, s2)) => prefix ^ "Rec(" ^ s1 ^")(" ^ refinedToString s2 ^ ")\n"
  | SyncR (Fail) => prefix ^ "Fail\n" 
  | TupleR reps => 
	let val ss = map (repToString (prefix ^ "    ")) reps 
	in
	   prefix ^ "Tuple {\n" ^
	   (String.concat ss) ^ 
	   prefix ^ "}\n"
	end
  | UnionR (branch, rep) =>
	   prefix ^ "Union (" ^ Int.toString branch ^") {\n" ^
	   (repToString (prefix ^ "    ") rep) ^
	   prefix ^ "}\n"
  | ArrayR (elems, seps, term) =>
      let 
	val elem_strings = map (fn r => prefix ^ "ELEM:\n" ^ (repToString (prefix ^ "    ") r)) elems
	val sep_strings = map (fn r => prefix ^ "SEP:\n" ^ (repToString (prefix ^ "    ") r)) seps
	val term_string = 
	  case term of 
	 	NONE => prefix ^ "TERM: Empty\n"
		| SOME term => prefix ^ "TERM:\n" ^ (repToString (prefix ^ "    ") term)
	val total_string = ListPair.foldl (fn (a, b, s) => s ^ a ^ b) 
			"" (elem_strings, sep_strings @ [term_string])
      in 
	prefix ^ "Array {\n" ^
	total_string
	^ prefix ^ "}\n"
      end
  | OptionR rep_opt =>
	prefix ^ "Option {\n" ^
	(
	case rep_opt of
	  NONE => prefix ^ "    " ^ "Empty\n"
	| SOME r => repToString (prefix ^ "    ") r 
	) ^ prefix ^ "}\n"

  | SwitchR (re, rep) =>
	   prefix ^ "Switch (" ^ refinedToString re ^") {\n" ^
	   (repToString (prefix ^ "    ") rep) ^
	   prefix ^ "}\n"
end
