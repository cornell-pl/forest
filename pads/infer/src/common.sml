(*l Defines utility functions common to all modules *)
structure Common = struct
	open Types (* defined in types.sml *)

	fun some(a : 'a option) : 'a = case a of SOME x => x | NONE => raise Size
	fun idcompare (id1:Id, id2:Id):order =
            String.compare(Atom.toString(id1), Atom.toString(id2))

	structure LabelMap = SplayMapFn(struct
                 type ord_key = Id
                 val compare = idcompare
        end)
	structure LabelSet = SplaySetFn(struct
                 type ord_key = Id
                 val compare = idcompare
        end)
	structure IntSet = SplaySetFn(struct
                 type ord_key =int 
                 val compare = Int.compare
        end)
   	structure IntMap = RedBlackMapFn(
                 struct type ord_key = int
	    		val compare = Int.compare
		 end) 

	exception TyMismatch
	exception RecordNum

	(*this function converts a given string to lower cases*)        
	fun toLower(s:string) : string = String.map Char.toLower s
	(* defines an arbitrary order on tokens	to put it in maps *)
	(* it overrides some of the ordering in structure.sml *)
	fun compare(a : Token option,b:Token option) = case (a,b) of
			(SOME a', SOME b') => compared(a',b')
		|	(SOME a, _) => GREATER
		|	(NONE, SOME _) => LESS
		|	(NONE, NONE) => EQUAL
		and compared(a,b) = case (a, b) of
			(Pint (x, _), Pint (x', _)) => LargeInt.compare(x,x')
			| (Pfloat x, Pfloat x') => 
				(
				case (x, x') of 
				  ((a, b), (a', b')) => if a=a' then String.compare (b, b')
				  			else LargeInt.compare(some(LargeInt.fromString(a)), 
							     some(LargeInt.fromString(a')))
				)
			| (Pstring (s1), Pstring(s2)) => String.compare(s1, s2)
			| (Ptime(s1), Ptime(s2)) => String.compare(s1, s2)
			| (Pdate(s1), Pdate(s2)) => String.compare(s1, s2)
			| (Pip(s1), Pip(s2)) => String.compare(s1, s2)
			| (Phostname(s1), Phostname(s2)) => String.compare(s1, s2)
			| (Ppath(s1), Ppath(s2)) => String.compare(s1, s2)
			| (Purl(s1), Purl(s2)) => String.compare(s1, s2)
			| (Pemail(s1), Pemail(s2)) => String.compare(s1, s2)
			| (Pmac(s1), Pmac(s2)) => String.compare(toLower s1, toLower s2)
			| (Pwhite(s1), Pwhite(s2)) => String.compare(s1, s2)
			| (Ptext(s1), Ptext(s2)) => String.compare(s1, s2)
			| (Other(c1), Other(c2)) => Char.compare(c1, c2)
			| _ => compToken(a, b)
	
	structure BDSet = RedBlackSetFn(struct
                                              type ord_key = Token 
                                              val compare = compared
                                        end)
	
	(* ____ to string functions useful for debugging *)

	fun printTyList tylist =
		case tylist of
			ty::tail => (printTy ty; printTyList tail)
			| nil => print "--- end of TyList ---\n"

	(* function to transpose a table *)
        fun transpose( alistlist : 'a list list) : 'a list list =
                let
                        fun addtohead(alist, alistlist) = case (alist,alistlist) of
                          (h::t, h2::t2) => (h :: h2) :: addtohead(t,t2)
                        | (nil,nil) => nil
                        | _  => raise TyMismatch
                in
                        List.foldr addtohead (List.map (fn x => nil) 
				(List.hd alistlist)) alistlist
                end
	datatype constraint =
	  Length of int                (* constrains array lengths, string lengths, # of int digits *)
	| Ordered of ordered             (* constrains arrays to be in order *)
	| Unique of Token (* value is always the same when present, and is Token *)
	| Range of LargeInt.int * LargeInt.int (* constrains integers to fall in this range, inclusive *)
	| Switched of Id list * (Token option list * Token option) list (* a mapping between ids in id list, their values in the list of Token options, and the value of this node *)
	| Eq of (Id * Rat.rat) list * Rat.rat (* lin equation of Id list plus constant *)
	| EnumC of BDSet.set (* set of values it takes on *)
	and ordered = Ascend | Descend

	fun tokenToRawString (d:Token):string = 
	  case d of
		PbXML(node, attrib) => "<" ^ node ^ " " ^ attrib ^ ">"
	|	PeXML(node, attrib) => "</" ^ node ^ " " ^ attrib ^ ">"
	|	Pint (i, s) => s
	|	Pfloat (a, b) =>  a ^"."^ b
	|	Ptime(t) => t
	|	Pdate(t) => t
	|	Pip(t)  => t
	|	Phostname(t)  => t
	|	Ppath(t)  => t
	|	Purl(t)  => t
	|	Pemail(t)  => t
	|	Pmac(t)  => (toLower t)
	|	Pstring(str)  => str
	|	Pwhite (str)  =>  str  
	|	Ptext (str)  =>  str  
	|	Other (c)  => str(c) 
	| 	Pempty => "" 
	|	_ => raise TyMismatch

	fun tokentorefine (d:Token):Refined =
		case d of
		PbXML(node, attrib) => StringConst(node ^ " + " ^ attrib) 
	|	PeXML(node, attrib) => StringConst(node ^ " + " ^ attrib) 
	|	Pint (i, _) => IntConst(i)
	|	Pfloat(t) => FloatConst(t)
	|	Ptime(t) => StringConst(t)
	|	Pdate(t) => StringConst(t)
	|	Pip(t)  => StringConst(t)
	|	Phostname(t)  => StringConst(t)
	|	Ppath(t)  => StringConst(t)
	|	Purl(t)  => StringConst(t)
	|	Pemail(t)  => StringConst(t)
	|	Pmac(t)  => StringConst((toLower t))
	|	Pstring(str)  => StringConst(str)
	|	Pwhite (str)  =>  StringConst(str)  
	|	Ptext (str)  =>  StringConst(str)  
	|	Other(c)  =>  StringConst(Char.toString(c))  
	| 	_ => StringConst("")

	fun bdoltos (x:Token option list): string = (case x of
		h :: nil => (case h of SOME a => tokenToString a | NONE => "NONE")
	|	h :: t => (case h of SOME a => tokenToString a | NONE => "NONE") 
				^ "\t" ^ (bdoltos t)
	|	nil => "()\n")

	fun idstostrs(idlist: Id list):string list =
            map (fn id => Atom.toString(id)) idlist
		
	(* link a list of labels with separators *)
	fun implode(slist : string list, seperator:string):string =
            case slist of
                 h :: nil => h
               | h :: t   => h ^ seperator ^ implode(t,seperator)
               | nil      => ""

	fun ctos(c:constraint):string = (case c of
		  Length x => "Length " ^ (Int.toString x)
		| Ordered x => (case x of Ascend => "Ascending" | Descend => "Decending")
		| Unique x => "Unique: " ^ (tokenToString x)
		| EnumC bdset => "EnumC:\n" ^ ( implode(map tokenToString (BDSet.listItems bdset),"\n") )
		| Range(l,h) => "Range [" ^ (LargeInt.toString l) ^ "," ^ 
				(LargeInt.toString h) ^ "]"
		| Eq(((lb,i) :: idlist), c) => "Equation " ^ (foldl (fn ((lb,i),str) => str ^ " + " ^ (Rat.toString i) ^ Atom.toString(lb)) ((Rat.toString i) ^ Atom.toString(lb)) idlist) ^ " + " ^ (Rat.toString c)
		| Eq(nil,c) => "Equation " ^ (Rat.toString c)
		| Switched (lablist, branches) => "Switched " ^ (
		let 
		val lab = implode(idstostrs(lablist),"\t") 
		val branches' = map (fn (bdolist,bdo) => "(" ^ bdoltos bdolist ^ ") -> " ^ bdoltos [bdo]) branches 
		val vals = implode(branches',"\n") 
		in 
		"(" ^ lab ^ ")\n" ^ vals 
		end)) ^ "\n"
	fun printConstMap (cmap:constraint list LabelMap.map):unit =
            LabelMap.appi (fn (lab,clist) => print (Atom.toString(lab) ^ ":\n" ^ (String.concat(map ctos clist))^ "\n")) cmap
	(*
	fun isIn(ch:char,str:string):bool =
            List.exists (fn x => x = ch) (String.explode str)
	fun escapeRegex(str:string):string =
            String.translate (fn x => if isIn(x ,"^$.[]|()*+?" )
                                      then "\\" ^ (String.str x) 
                                      else String.str x) str
	*)
	(* function to escape the backslashes in a re string *)
	fun escapeRegex (re:string) = 
		String.translate (fn x => 
		if x = #"\\" then "\\\\" 
		else if x = #"\"" then "\\\""
		else String.str x) re
 
	fun myand(a,b) = a andalso b
	fun myor(a,b) = a orelse b
	fun ltoken_equal((tk1, _):LToken, (tk2, _):LToken):bool =
	  case (tk1, tk2) of 
		    (PbXML(a,b), PbXML(a1, b1)) => (a=a1 andalso b = b1)
		  | (PeXML(a,b), PeXML(a1, b1)) =>  (a=a1 andalso b = b1) 
		  | (Ptime(a), Ptime(b)) => (a = b)
		  | (Pdate(a), Pdate(b)) => (a = b)
		  | (Pip(a), Pip(b)) => (a = b)
		  | (Phostname(a), Phostname(b)) => (a = b)
		  | (Ppath(a), Ppath(b)) => (a = b)
		  | (Purl(a), Purl(b)) => (a = b)
		  | (Pemail(a), Pemail(b)) => (a = b)
		  | (Pmac(a), Pmac(b)) => ((toLower a) = (toLower b))
		  | (Pint(a, s1), Pint(b, s2)) => (a = b andalso s1 = s2)
		  | (Pfloat(a), Pfloat(b)) => (a = b)
		  | (Pstring(a), Pstring(b)) => (a = b)
		  | (Pwhite(a), Pwhite(b)) => (a = b)
		  | (Other(a), Other(b)) => (a = b)
		  | (Ptext(a), Ptext (b)) => (a = b)
		  | (Pempty, Pempty) => true
		  (* ignoring Pgroup for now *)
		  | _ => false
	fun ltoken_ty_equal ((tk1, _):LToken, (tk2, _):LToken):bool =
	  if compToken (tk1, tk2) = EQUAL then true
	  else false
	fun ltokenlToRefinedOp ltokenl=
		case ltokenl of
		  h::t =>
		  let 
			val not_equal = (List.exists (fn x => not (ltoken_equal(x, h))) t)
		  in
			if not_equal then 
			  case h of 
			    (Pwhite _, _) => SOME (StringME ("/[ \\t\\r\\n]+/"))
			  | (Pint _, _) => SOME (StringME ("/\\-?[0-9]+/"))
			  | _ => NONE 
			else SOME (tokentorefine (#1 (hd ltokenl)))
		  end
		  | nil => NONE


    (*function to merge AuxInfo a1 into a2*)
    fun mergeAux(a1, a2) =
	case (a1, a2) of 
	 ({coverage=c1, label=l1, tycomp=tc1},{coverage=c2, label=l2, tycomp=tc2}) =>
 	 	{coverage=c1+c2, label=l2, tycomp=tc2} (* ????? *)

    (*this function generate a dummy empty base type with nTokens number of Pempty tokens *)
    fun genEmptyBase aux nTokens =
	let
	   val ltokens = List.tabulate(nTokens, (fn n => (Pempty, {lineNo=(~1), beginloc=0, endloc=0,recNo=(~1)})))
	   val emptyBase = Base(aux, ltokens)
	in emptyBase
	end
    (*function that test if tylist1 in a struct can be described by tylist2 in another struct*)
    (* tylist1 is described by tylist2 if tylist1 is a sub-sequence of tylist2 and 
	all other elements in tylist2 can describe Pempty *)
    fun listDescribedBy (tylist1, tylist2) = 
      let
	 val (len1, len2) = (length(tylist1), length(tylist2))
      in
	(len1 <= len2) andalso
	let 
	   val head2 = List.take(tylist2, len1)
	   val tail2 = List.drop(tylist2, len1)
	   val emptyBase = genEmptyBase (getAuxInfo (hd tylist1)) 1
	in
	   (
	   (foldr myand true (map describedBy (ListPair.zip (tylist1, head2)))) 
	   andalso (*the tail2 all describe Pempty *)
	   (foldr myand true (map (fn x => describedBy (emptyBase, x)) tail2)) 
	   )
	   orelse (describedBy(emptyBase, hd tylist2) andalso 
	   	listDescribedBy (tylist1, List.drop(tylist2, 1)))
	end
      end
    (*function that test if ty1 can be described by ty2 *)
    (*TODO: not considering Parray and RArray for now *)
    and describedBy(ty1, ty2) =
	let
	  val emptyBase = genEmptyBase (getAuxInfo ty1) 1
	  val res =
	    case (ty1, ty2) of 
		(*assume no Pempty in the Pstruct as they have been cleared by remove_nils*)
		(Base(a1, tl1), Base(a2, tl2)) => ltoken_ty_equal(hd tl1, hd tl2)
		| (Base(a1, tl1), Pstruct(a2, tylist2)) => listDescribedBy ([Base(a1, tl1)], tylist2)
		(*below is not completely right, haven't considered the case of tylist1 is a subset
		  of tylist2 and the rest of tylist2 can describe Pempty *) 
		| (Pstruct(a1, tylist1), Pstruct(a2, tylist2)) => listDescribedBy(tylist1, tylist2)
		| (Punion(a1, tylist1), Punion(a2, tylist2)) =>
			foldr myand true (map 
				(fn ty => (foldr myor false (map (fn x => describedBy (ty, x)) tylist2))) 
				tylist1)
		| (ty1, Punion(a2, tylist2)) =>
			foldr myor false (map (fn x => describedBy (ty1, x)) tylist2)
		| (Poption(a1, ty), ty2) => describedBy (emptyBase, ty2) andalso describedBy (ty, ty2)
		| (ty1, Poption(a2, ty)) => describedBy (ty1, emptyBase) orelse describedBy (ty1, ty)
		(*
		| (Switch(a1, id1, rtylist1), Switch(a2, id2, rtylist2)) =>
			Atom.same(id1, id2) andalso 
			(foldr myand true (map (fn x => rtyexists (x,rtylist2)) rtylist1))
		*)
		| _ => false
(*
	    val _ = (print "Checking\n"; printTy(ty1); print "with ...\n"; printTy(ty2); 
			print "Answer is: "; (if res = true then print "true\n\n" else print "false\n\n"))
*)
	in res
	end

    
    (*merge a ty into a tylist in a union *)
    fun mergeUnion (ty, tylist, newlist) = 
      case tylist of 
	h::tail => if (describedBy (ty, h)) then newlist@[mergeTyInto(ty, h)]@tail
		   else (mergeUnion (ty, tail, newlist@[h]))
	| nil => newlist
    and describesEmpty tylist =
      case tylist of 
      nil => true
      | h::t =>
	let
	   val emptyBase = genEmptyBase (getAuxInfo (hd tylist)) 1
	in
	   foldr myand true (map (fn x => describedBy (emptyBase, x)) tylist) 
	end handle Empty => false
    (*function to merge one list in struct to another list in struct*)
    and mergeListInto (tylist1, tylist2, headlist) =
	let 
	   val (len1, len2) = (length(tylist1), length(tylist2))
	   val head2 = List.take(tylist2, len1)
	   val tail2 = List.drop(tylist2, len1)
	in
	   if (describesEmpty headlist andalso 
	       foldr myand true (map describedBy (ListPair.zip (tylist1, head2))) andalso 
	       describesEmpty tail2) (*found the merging point*)
	   then
		let
	          (*here need to push a base with correct number of Pempty tokens into the head and tail lists
			note that the recNo of those "fake" tokens will be -1 and will not be used in
			table generation *)
	   	  val emptyBase = genEmptyBase (getAuxInfo(hd tylist1)) (getCoverage (hd tylist1))
		  fun pushInto ty tylist = map (fn t => mergeTyInto (ty, t)) tylist
		in
		  (pushInto emptyBase headlist)@(map mergeTyInto (ListPair.zip (tylist1, head2)))@
		  (pushInto emptyBase tail2)	
		end
	   else 
		if len2 <= 1 then
		  (print "Two tylists don't match!\n"; raise TyMismatch)
		else mergeListInto (tylist1, List.drop(tylist2, 1), headlist@[hd tylist2])
	end
    (*function to merge ty1 and ty2 if ty1 is described by ty2 *)
    (*this function is used in refine_array rewriting rule, the recNo in ty1 
	is updated so that they are consistent with ty2 *)
    (*TODO: not considering Parray and RArray for now *)
    and mergeTyInto (ty1, ty2) =
		case (ty1, ty2) of 
		(Base(a1, tl1), Base(a2, tl2)) => Base(mergeAux(a1, a2), tl2@tl1) 
		| (Base(a1, tl1), Pstruct(a2, tylist2)) => Pstruct(mergeAux(a1, a2), 
			mergeListInto([Base(a1, tl1)], tylist2, nil))
		(*below is not completely right, haven't considered the case of tylist1 is a subset
		  of tylist2 and the rest of tylist2 can describe Pempty *) 
		| (Pstruct(a1, tylist1), Pstruct(a2, tylist2)) => 
			Pstruct(mergeAux(a1, a2), mergeListInto(tylist1, tylist2, nil))
		| (Punion(a1, tylist1), Punion(a2, tylist2)) => foldl mergeTyInto ty2 tylist1
		| (Poption (a1, ty), ty2) => 
			let
			  val emptyCoverage = getCoverage ty1 - getCoverage ty
			in
			  mergeTyInto ((genEmptyBase (mkTyAux emptyCoverage) emptyCoverage), mergeTyInto (ty, ty2))
			end
		| (ty1, Poption (a2, ty2)) =>
			if (describesEmpty [ty1]) then Poption(mergeAux(getAuxInfo(ty1), a2), ty2)
			else Poption(mergeAux(getAuxInfo(ty1), a2), mergeTyInto(ty1, ty2))
		| (ty1, Punion (a2, tys)) =>
			Punion (mergeAux (getAuxInfo ty1, a2), mergeUnion (ty1, tys, nil))
		(*
		| (Switch(a1, id1, rtylist1), Switch(a2, id2, rtylist2)) =>
			Atom.same(id1, id2) andalso 
			(foldr myand true (map (fn x => rtyexists (x,rtylist2)) rtylist1))
		*)
		| _ => (print "mergeTyInto error!\n"; printTy ty1; printTy ty2; raise TyMismatch)

(*this function insert all the rec no of a ty into a map*)
	fun insertToMap ty intmap =
	let
		fun insertTListToMap tl intmap =
			case tl of
				nil => intmap
				| (t, {lineNo, beginloc, endloc, recNo}) :: ts => 
					insertTListToMap ts (IntMap.insert(intmap, recNo, 0))
		fun insertLensToMap lens intmap =
			case lens of 
				nil => intmap
				| (l, r) :: ls => insertLensToMap ls (IntMap.insert(intmap, r, 0))
	in
	  case ty of
		Base(_, tl)=> insertTListToMap tl intmap
		| RefinedBase (_, _, tl) => insertTListToMap tl intmap
		| TBD _  => intmap
		| Bottom _ => intmap
		| Punion(_, tylist)=> foldr (fn (x, m) => insertToMap x m) intmap tylist
		| Pstruct(_, tylist)=> foldr (fn (x, m) => insertToMap x m) intmap tylist
		(*inside body is another scope *)
		| Parray(_, {tokens, lengths, first, body, last}) =>
			 foldr (fn (x, m) => insertToMap x m) 
				(insertLensToMap lengths intmap) [first,last]
		(*inside RArray body is another scope*)
		| RArray(_, _, _, ty, _, lens) => insertLensToMap lens intmap 
		| Switch (a, i, rtl) => foldr (fn ((r, ty), m) => insertToMap ty m) intmap rtl
		| Poption (a, ty) => insertToMap ty intmap
	end
	(*this function reindex the recNo by collapsing them in every token of a given ty 
	and a start index and returns the updated ty *)
	fun reIndexRecNo ty startindex = 
	  let
(*
		val _ = print ("startindex = "^(Int.toString startindex)^" and The ty is\n")
		val _ = printTy ty
*)
		fun updateMap intmap =
		  let
			val pairs = IntMap.listItemsi intmap
		    	fun insertPairs pairs index intmap =
			  case pairs of 
			    nil => intmap
			    | (oldRecNo, _)::rest => insertPairs rest (index+1) 
						(IntMap.insert(intmap, oldRecNo, index))
		  in
			insertPairs pairs startindex intmap
		  end
		fun updateTL(intmap, tl, newtl) =
			case tl of 
			  nil => newtl
			  | (t, {lineNo, beginloc, endloc, recNo})::ts => 
				let
				  val newRecop = IntMap.find(intmap, recNo)
				in
				  case newRecop of
					NONE => (print ("recNum " ^ Int.toString recNo ^
						" not found!\n"); raise RecordNum)
					| _ => updateTL(intmap, ts, newtl@[(t, {lineNo=lineNo, beginloc=beginloc, 
							endloc=endloc, recNo=some(newRecop)})])
				end
		fun updateLens intmap lengths =
			case lengths of
			  nil => nil
			  | (l, r)::tail => 
				let
				  val newRecOp = IntMap.find (intmap, r)
				in
			  	  case newRecOp of
					NONE => (print ("recNum " ^ Int.toString r ^
						" not found!\n"); raise RecordNum)
					| _ => (l, (some newRecOp))::(updateLens intmap tail)
				end
		fun updateTy intmap ty =
		  case ty of
			Base(a, tl)=> (Base(a, updateTL(intmap, tl, nil)))
			| RefinedBase (a, r, tl) => (RefinedBase(a, r, updateTL(intmap, tl, nil)))
			| Punion(a, tylist)=> Punion(a, map (updateTy intmap) tylist)
			| Pstruct(a, tylist)=> Pstruct(a, map (updateTy intmap) tylist)
			| Parray(a, {tokens, lengths, first, body, last}) => Parray(a, {tokens = tokens,
					lengths = (updateLens intmap lengths), first = updateTy intmap first, 
					body = body, last = updateTy intmap last})
			| RArray(a, s, t, body, l, lens) => RArray(a, s, t, body, l, 
					(updateLens intmap lens)) 
			| Switch (a, i, rtl) => Switch(a, i, map (fn (r, t) => (r, updateTy intmap ty)) rtl)
			| Poption(a, ty') => Poption(a, updateTy intmap ty')
			| _ => ty

		val recNoMap = insertToMap ty IntMap.empty

	  in
		updateTy (updateMap recNoMap) ty	
	  end

    (*function to merge two tys that are equal structurally*)
    (*merge ty1 into ty2, assuming ty1 and ty2 are in the same scope*)
    (*used by refine_array *)
    fun mergeTy (ty1, ty2) =
	case (ty1,ty2) of
		(Base(a1, tl1), Base (a2, tl2)) => Base (mergeAux(a1, a2), tl1@tl2)
		| (RefinedBase (a1, r1, tl1), RefinedBase(a2, r2, tl2)) => 
						RefinedBase(mergeAux(a1, a2), r1, tl1@tl2)
		| (TBD (a1, s1, cl1), TBD (a2, s2, cl2)) => TBD (mergeAux(a1, a2), s1, cl1@cl2)
		| (Bottom (a1, s1, cl1), Bottom (a2, s2, cl2)) => Bottom (mergeAux(a1, a2), s1, cl1@cl2)
		| (Punion(a1, tylist), Punion(a2, tylist2)) => Punion(mergeAux(a1, a2), 
				map mergeTy (ListPair.zip(tylist,tylist2)))
		| (Pstruct(a1, tylist), Pstruct(a2, tylist2)) => Pstruct(mergeAux(a1, a2),
				map mergeTy (ListPair.zip(tylist,tylist2)))
		| (Parray(a1, {tokens=t1, lengths=len1, first=f1, body=b1, last=l1}), 
		   Parray(a2, {tokens=t2, lengths=len2, first=f2, body=b2, last=l2})) => 
			(*body is in a different scope so reindex*)
			Parray(mergeAux(a1, a2), {tokens = t1@t2, lengths = len1@len2, 
			first = mergeTy(f1, f2),
			body = mergeTy((reIndexRecNo b1 (getCoverage b2)), b2),
			last = mergeTy(l1, l2)})
		| (Switch (a1, id1, rtylist1), Switch(a2, id2, rtylist2)) =>
			let val (rl1, tylist1) = ListPair.unzip (rtylist1)
			    val (rl2, tylist2) = ListPair.unzip (rtylist2)
			in
			    if (Atom.same(id1, id2) andalso
					foldr myand true (ListPair.map refine_equal(rl1, rl2)) )
			    then
				Switch(mergeAux(a1, a2), id1, 
					ListPair.zip(rl1, map mergeTy (ListPair.zip(tylist1, tylist2))))
			    else raise TyMismatch
			end
		| (RArray(a1, sepop1, termop1, ty1, len1, l1), 
			RArray (a2, sepop2, termop2, ty2, len2, l2))
			(*body is in a different scope so reindex*)
			=> RArray(mergeAux(a1, a2), sepop1, termop1, 
				mergeTy((reIndexRecNo ty1 (getCoverage ty2)), ty2), len1, (l1@l2)) 
		| (Poption(a1, ty1), Poption (a2, ty2)) =>
		    	Poption (mergeAux(a1, a2), mergeTy (ty1, ty2))	
		| _ => (print "The following tys don't match!!\n"; printTy ty1; 
			printTy ty2; raise TyMismatch)

    (* function to test of two ty's are completely equal minus the labels *)
    (* if comparetype = 0, compare everything, otherwise compare down to 
	base modulo the token list and other meta data *)
    (* comparetype = 0 is currently not used and is not fully implemented *)
    fun ty_equal (comparetype:int, ty1:Ty, ty2:Ty):bool = 
	let
		fun check_list(l1:Ty list,l2: Ty list):bool = 
		if length l1 <> length l2 then false
		else
		  let 
			val bools = ListPair.map (fn (t1, t2) => ty_equal(comparetype, t1, t2)) 
			(l1, l2)
		  in
			foldr myand true bools
		  end
	in
		case (ty1,ty2) of
			(Base(_, tl1), Base (_, tl2)) => 
				if (comparetype = 0) then
				foldr myand true (ListPair.map ltoken_equal (tl1, tl2))
				else ltoken_ty_equal(hd tl1, hd tl2)
			| (TBD _, TBD _) => true
			| (Bottom _, Bottom _) => true
			| (Punion(_, tylist), Punion(_, tylist2)) => check_list(tylist,tylist2)
			| (Pstruct(_, tylist), Pstruct(_, tylist2)) => check_list(tylist,tylist2)
			| (Parray(_, a1), 
			   Parray(_, a2)) => ty_equal(comparetype, #first a1, #first a2) andalso 
				ty_equal(comparetype, #body a1, #body a2) andalso 
				ty_equal(comparetype, #last a1, #last a2) 
			| (RefinedBase (_, r1, tl1), RefinedBase(_, r2, tl2)) => 
				if (comparetype = 0) then
				(refine_equal(r1, r2) andalso 
				foldr myand true (ListPair.map ltoken_equal(tl1, tl2)))
				else refine_equal(r1, r2)
			| (Switch (_, id1, rtylist1), Switch(_, id2, rtylist2)) =>
				let val (rl1, tylist1) = ListPair.unzip (rtylist1)
				    val (rl2, tylist2) = ListPair.unzip (rtylist2)
				in
				        Atom.same(id1, id2) andalso 
					foldr myand true (ListPair.map refine_equal(rl1, rl2)) 
					andalso check_list (tylist1, tylist2)
				end
			| (RArray(_, sepop1, termop1, ty1, len1, _), 
				RArray (_, sepop2, termop2, ty2, len2, _))
				=> refine_equal_op1(sepop1, sepop2) andalso
				   refine_equal_op1(termop1, termop2) andalso
				   ty_equal (comparetype, ty1, ty2) andalso
				   refine_equal_op1(len1, len2)
			| (Poption (_, t1), Poption (_, t2)) => 
				ty_equal (comparetype, t1, t2)
			| _ => false 
		handle Size => (print "Size in ty_equal!\n" ; false)
	end


	fun mergeTyForArray (ty1, ty2) = mergeTy (ty1, (reIndexRecNo ty2 (getCoverage ty1)))

	fun printIntSet intset =
	  let
	    val vals = IntSet.listItems intset
	    fun listToString l =
		case l of
		nil => "\n"
		| x::tail => ((Int.toString x) ^ " " ^ (listToString tail))
	  in print (listToString vals)
	  end

	(*this function clean up the Punion structure so that Pempty if it exists 
	always appear last in a union*)
	fun cleanupUnion unionTy =
	let 
	in
	  case unionTy of
	    Punion (a, tys) => 
		let
		
		  fun isNotPempty ty =
		    case ty of
			  Base (_, ltokens) => 
			    (case (hd ltokens) of 
			     (Pempty, _) => false
			     | _ => true)
			 | _ => true 
		  fun isPempty ty = (not (isNotPempty ty))
		  val nonPemptyTys = List.filter isNotPempty tys
		  val emptys =List.filter isPempty tys
		  val emptys' = if (length emptys) = 0 then []
				else if (length emptys) = 1 then emptys
				else [foldr mergeTy (hd emptys) (List.drop (emptys, 1))]
		in
		  Punion (a, nonPemptyTys@emptys')
		end
	    | _ => raise TyMismatch
	end

        (* this function checks if the given ty can parse an empty string *)
	fun parseEmpty ty = 
	  case ty of 
	    Base (_, (Pempty, _)::_) => true
	  | RefinedBase (_, StringConst "", _) => true
	  | RefinedBase(_, Blob _, _) => true
	  | Pstruct (_, tys) =>  
	      foldr myand true (map (fn ty => parseEmpty ty) tys) 
	  | Punion (_, tys) => 
	      foldr myor false (map (fn ty => parseEmpty ty) tys) 
	  | Switch (a, id, pairs) => 
	      foldr myor false (map (fn (_, ty) => parseEmpty ty) pairs) 
	  | Parray _ => true
	  | RArray _ => true
	  | Poption _ => true
	  | _ => false

	(*This function sorts of the base type union branches by the order defined in tokens.sml*)
	(*assume ty is already with complexy info*)
	fun sortUnionBranches ty =
		case ty of 
		  Punion(a, tys) =>
			let
			  val sorted_tys = map sortUnionBranches tys
			  fun isPriTy ty =
				case ty of
					Base(a1, (Ptime _, _)::_) => true
					| Base(a1, (Pdate _, _)::_) => true
					| Base(a1, (Pip _, _)::_) => true
					| Base(a1, (Phostname _, _)::_) => true
					| Base(a1, (Ppath _, _)::_) => true
					| Base(a1, (Purl _, _)::_) => true
					| Base(a1, (Pemail _, _)::_) => true
					| Base(a1, (Pmac _, _)::_) => true
					| Base(a1, (Ptext _, _)::_) => true
(*					| RefinedBase(_, IntConst x , _) => x > 99 *)
					| RefinedBase(_, StringConst s , _) => (size s) > 2
					| _ => false
			  fun isNotPriTy ty = not (isPriTy ty)
			  fun lowPriTy ty = 
				(parseEmpty ty) orelse
				(
				case ty of
					Base(_, (Pint _, _)::_) => true
					| Base(_, (Pstring _, _)::_) => true
					| Base(_, (Other _, _)::_) => true
					| RefinedBase(_, StringConst s , _) => (size s) = 1
					| _ => false
				)
			  fun notLowPriTy ty = not (lowPriTy ty)

			  val priTys = List.filter isPriTy sorted_tys
			  val nonpriTys = List.filter isNotPriTy sorted_tys
			  val lowPriTys = List.filter lowPriTy nonpriTys
			  val normalTys = List.filter notLowPriTy nonpriTys
			  fun greater (ty1, ty2) =
			    let
				val (cov1, cov2) =(getCoverage ty1, getCoverage ty2)
				(*
				val (comps1, comps2) = (getComps ty1, getComps ty2)
				val (nc1, nc2) = ((normalizeTyComp cov1 comps1), (normalizeTyComp cov2 comps2))
				*)
			    in
				case (ty1, ty2) of
				  (Base(a1, (tok1, _)::t1), Base(a2, (tok2, _)::t2)) => 
					(compToken(tok1, tok2) = GREATER)
				  | (RefinedBase (a1, re1, t1), RefinedBase(a2, re2, t2)) =>
					(case (re1, re2) of
						(StringConst x, StringConst y) => (size x < size y)
						| (IntConst x, IntConst y) => x < y
						| _ => false
					)
				  | (Base (_, (Pempty, _)::_), _) => true
				  | (_, Base (_, (Pempty, _)::_)) => false
				  | (Base _, RefinedBase _) => true
				  | (RefinedBase _, Base _) => false
				  | (Base (a1, t1), _) => (case hd t1 of (Pstring x, _) => true | _ => false)
				  | (_, Base(a1, t1)) => (case hd t1 of (Pstring x, _) => false | _ => true)
				  | (RefinedBase _, _) => (cov1 > cov2) 
				  | (_, RefinedBase _) => cov1 > cov2
				  | _ => cov1 > cov2 
			    end
			  val sortedPriTys = ListMergeSort.sort greater priTys
			  val sortedLowPriTys = ListMergeSort.sort greater lowPriTys
			in Punion(a, sortedPriTys@normalTys@sortedLowPriTys)
			end
		  | Pstruct(a, tys) => Pstruct(a, map sortUnionBranches tys)
		  | RArray (a, sep, term, body, len, lengths) => RArray(a, sep, term, sortUnionBranches body, len, lengths)
		  | Switch (a, id, pairs) => 
			let
                          val (refs, tylist) = ListPair.unzip(pairs)
                          val tylist' = map sortUnionBranches tylist
                        in Switch (a, id, ListPair.zip(refs, tylist'))
                        end
		  | Poption (a, body) => Poption (a, sortUnionBranches body)
		  | _ => ty

(* remove all redundant Pemptys from struct *)
fun removePempty ty =
case ty of
  Pstruct (a, tys) =>
    let		
	val tys' = map removePempty tys
    in
	case tys' of
	  nil => ty
	| _ =>
	  let
            fun isNotPempty ty =
		case ty of
		  Base (_, ltokens) => 
		    (case (hd ltokens) of 
		     (Pempty, _) => false
		     | _ => true)
		 | _ => true 
	    val nonEmpties = List.filter isNotPempty tys' 
          in
	    case nonEmpties of
	      nil => hd tys'
	    | _ => Pstruct (a, nonEmpties)
          end
    end
  | Punion (a, tys) => Punion (a, map removePempty tys)
  | Parray (a, {tokens=t, lengths = l, first=f, body=b, last=la}) =>
	Parray (a, {tokens=t, lengths=l, first=removePempty f, 
			body=removePempty b, last=removePempty la})
  | _ => ty

  (*here we choose an arbitrary measure of type complexity to order the tys 
  if they are not equal *)
  fun tycompare (ty1: Ty, ty2: Ty) : order =
	if ty_equal (1, ty1, ty2) then EQUAL
	else let val comp1 = getTypeComp ty1
		 val comp2 = getTypeComp ty2
	     in if (Complexity.toReal comp1) <(Complexity.toReal comp2) then LESS
		else GREATER	
	     end
  structure TyMap = ListMapFn(
    struct type ord_key = Ty
  	   val compare = tycompare
    end) 

end
