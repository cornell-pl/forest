(* Kenny Zhu 
   Handles the creation of tables from Ty stuctures so they can be used in 
   constraint analysis
*)
structure Table = struct
	open Common
	structure LMap = LabelMap
	exception TyMismatch
	exception RecordNum
	exception MissingId

	(*an infertable is a table of Tokens plus headers*)
	type infertable = (Id list) * (Token option list list)

	(* append two infertables, i.e. append the headers and append the body 
	*)
	fun appendtab ((hdrs1, body1):infertable, (hdrs2, body2):infertable):infertable = 
		(hdrs1 @ hdrs2, body1 @ body2)
	fun appendsettab ((set1, (hdrs1, body1):infertable), (set2, (hdrs2, body2):infertable)) = 
		(IntSet.union(set1, set2), (hdrs1 @ hdrs2, body1 @ body2))


        fun printHeaders(hlist) = (List.app (fn x => print (Atom.toString(x) ^ "\t" )) 
		hlist; print "\n")
	fun printTable (table) =
		case table of (hdrs, rows) =>
		  (
		    	printHeaders(hdrs);
		  	(List.app (fn x => print ((bdoltos x) ^ "\n")) rows)
		  )

	(*given an infertable (without headers), 
	  find out if record n exists in that table
	  i.e. whether row n of the table contains any token*)
	fun existsRecord(cols:Token option list list, n:int):bool =
		case cols of
			[] => false
			| column::remainder =>
				if List.nth (column, n) <> NONE then true
				else existsRecord(remainder, n)

	(* this function generate a column in the resulting table from 
		a Token list and fill NONEs to empty cells
	   if the Token list is longer than the stipulated size of the column
	   it will truncate the extra tokens from the list *)
	fun gencolumn (ltokens, columnsize) = 
	  let
		fun insertTok (ltoken:LToken, (map, set)) = 
			let
			  val recNo = (#recNo (#2 ltoken)) 
			in (IntMap.insert (map, recNo, (#1 ltoken)), IntSet.add (set, recNo))
			end
		val (recNoMap, recNoSet) = List.foldr insertTok (IntMap.empty, IntSet.empty) ltokens
		fun getTokenfromMap map recnum = IntMap.find (map, recnum)
	  in (recNoSet, List.tabulate(columnsize, (getTokenfromMap recNoMap)))
	  end

	fun intToToken (i:int):Token = Pint(Int.toLarge i, Int.toString i)

	fun genintcolumn(lints, columnsize) =
		(*get a token from a specfic record no from the int list
		  if that record no doesn't exist, return NONE*)
		let fun getIntfromList tlist recnum= 
			case tlist of 
			  [] => NONE
			| (arraysize:int, recordnum:int) :: tail =>
				if recordnum = recnum then 
				  SOME (intToToken(arraysize)) 
				else getIntfromList tail recnum
		in List.tabulate(columnsize, (getIntfromList lints))
		end

	(* returns a set of recNos under this Ty and an infertable *)
	fun genTable totalrecords ty = 
	case ty of
		Base (a, ltokenl) =>
			let 
(*
				val _ = print ("Gen column for "^ (getLabelString a) ^ "... ")
*)
				val (recNoSet, col) = gencolumn(ltokenl, totalrecords)
(*
				val _ = print ("done.\n")
*)
			in (recNoSet, ([some(#label a)], [col]):infertable)
			end
		| TBD _ => (IntSet.empty, (nil, nil) : infertable)
		| Bottom _ => (IntSet.empty, (nil, nil): infertable)
		| Pstruct (a, l) => 
		    let
			val pairs = map (genTable totalrecords) l
			val (recNoSets, tables) =  (ListPair.unzip pairs)
		    in
			(* the list of recNoSets should be the same at this point as this is a struct *)
			(hd recNoSets, List.foldr appendtab (nil, nil) tables)
		    end
		(* for Punion need to add a col for branching choices *)
		| Punion (a, l) => 
			let 
			  val pairs = (map (genTable totalrecords) l)
			  val (recNoSets, tables) =  (ListPair.unzip pairs)
			  (*TODO: exception with take *)
			  (* given a list of recNoSets and a record number, return
			  the index of the infertable that contains that record starting from 1 or NONE *)
			  fun g sets index n = 
				((*print ("g on " ^ Int.toString (n) ^ "\n"); *)
				case sets of 
				  [] => NONE
				  | s::tail => 
				    if IntSet.member(s, n) = true then SOME (intToToken(index))
				    else g tail (index+1) n
				)
			  val branchcol = List.tabulate(totalrecords, (g recNoSets 1))
			in
		   	  List.foldr appendsettab (IntSet.empty, ([some(#label a)], [branchcol])) pairs
			end
		| Parray (a, {tokens=_, 
			lengths= lens, first = fty, body=bty, last=lty}) =>
			List.foldr appendsettab (IntSet.empty, (nil, nil)) 
				[(IntSet.empty, ([some(#label a)], [genintcolumn(lens, totalrecords)])),
				 genTable totalrecords fty, genTable totalrecords lty]
		| RArray (a, _, _, _, _, lens) => 
			(IntSet.empty, ([some(#label a)], [genintcolumn(lens, totalrecords)]))
		| Poption (a, ty') => genTable totalrecords ty'
		| _ => (IntSet.empty, (nil, nil):infertable)

	fun parseArrays ty =
	  case ty of
		Base _ => []
	 	| TBD _ => []
		| Bottom _ => []
		| Pstruct (_, l) => List.concat (map parseArrays l)
		| Punion (_, l) => List.concat (map parseArrays l)
		| Parray(_, {tokens=_, 
			lengths= lens, first = fty, body=bty, last=lty}) =>
			(parseArrays fty) @ [bty] @ (parseArrays bty) @ (parseArrays lty)
		| RefinedBase _ => []
		| RArray (_, _, _, body, _, _) => [body] @ (parseArrays body)
		| Poption (_, body) => parseArrays body
		| _ => raise TyMismatch
end
