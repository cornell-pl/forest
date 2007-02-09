(* Kenny Zhu 
   Handles the creation of tables from Ty stuctures so they can be used in 
   constraint analysis
*)
structure Table = struct
	open Common
	structure LMap = Label.Map
	exception IRMismatch

	(*an infertable is a table of Tokens plus headers*)
	type infertable = Id option list * (Token option list list)

	(* append two infertables, i.e. append the headers and append the body 
	*)
	fun appendtab ((hdrs1, body1):infertable, (hdrs2, body2):infertable):infertable = 
		(hdrs1 @ hdrs2, body1 @ body2)

	(* function to transpose a table *)
        fun transpose( alistlist : 'a list list) : 'a list list =
                let
                        fun addtohead(alist, alistlist) = case (alist,alistlist) of
                          (h::t, h2::t2) => (h :: h2) :: addtohead(t,t2)
                        | (nil,nil) => nil
                        | _                        => raise IRMismatch
                in
                        List.foldr addtohead (List.map (fn x => nil) 
				(List.hd alistlist)) alistlist
                end


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
		a Token list and fill NONEs to empty cells *)
	fun gencolumn (ltokens, columnsize) = 
		(*get a token from a specfic line no from the token list
		  if that line no doesn't exist, return NONE*)
		let fun getTokenfromList tlist lineno = 
			case tlist of 
			  [] => NONE
			| (token, loc:location) :: tail =>
				if #lineNo loc = lineno then SOME token
				else getTokenfromList tail lineno
		in List.tabulate(columnsize, (getTokenfromList ltokens))
		end

	fun intToToken (i:int):Token = Pint(Int.toLarge i)

	fun genintcolumn(lints, columnsize) =
		(*get a token from a specfic line no from the token list
		  if that line no doesn't exist, return NONE*)
		let fun getIntfromList tlist lineno = 
			case tlist of 
			  [] => NONE
			| (arraysize:int, linenum:int) :: tail =>
				if linenum = lineno then 
				  SOME (intToToken(arraysize)) 
				else getIntfromList tail lineno
		in List.tabulate(columnsize, (getIntfromList lints))
		end
	
	fun genTable totalrecords ty : infertable = case ty of
		Base (a, ltokenl) =>
			let val col = gencolumn(ltokenl, totalrecords)
			in ([#label a], [col]):infertable
			end
		| Pvoid _ => (nil, nil): infertable
		| TBD _ => (nil, nil) : infertable
		| Bottom _ => (nil, nil): infertable
		| Pstruct (a, l) => 
			List.foldr appendtab (nil, nil) (map (genTable totalrecords) l)
		(* for Punion need to add a col for branching choices *)
		| Punion (a, l) => 
			let 
			  val tablelist = (map (genTable totalrecords) l)
			  (* given a list of infer tables, an index (starting from 1) 
			  and a record number, return
			  the index of the infertable that contains that record or NONE *)
			  fun g tablist index n = 
				case tablist of 
				  [] => NONE
				  | (_, cols)::tail => 
				    if existsRecord(cols, n) = true then SOME (intToToken(n))
				    else g tail (index+1) n
			  val branchcol = List.tabulate(List.length(#2 (List.nth(tablelist, 0))), 
					(g tablelist 1))
			in
		   	  List.foldr appendtab ([#label a], [branchcol]) tablelist
			end
		| Parray (a, {tokens=_, 
			lengths= lens, first = fty, body=bty, last=lty}) =>
			let 
				val lencol = ([#label a], [genintcolumn(lens, totalrecords)])
				val firsttab = genTable totalrecords fty
				(* currently we will truncate the first totalrecords from 
				   the token list from the body ty
				*)
				val bodytab = genTable totalrecords bty
				val lasttab = genTable totalrecords lty
			in List.foldr appendtab (nil, nil) 
				([lencol] @ [firsttab] @ [bodytab] @ [lasttab])
			end

end
