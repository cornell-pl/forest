(* Defines utility functions common to all modules *)
structure Common = struct
	open Types (* defined in types.sml *)

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
	exception TyMismatch

	(* defines an arbitrary order on tokens	to put it in maps *)
	(* it overrides some of the ordering in structure.sml *)
	fun compare(a : Token option,b:Token option) = case (a,b) of
			(SOME a', SOME b') => compared(a',b')
		|	(SOME a, _) => GREATER
		|	(NONE, SOME _) => LESS
		|	(NONE,NONE) => EQUAL
		and compared(a,b) = case (a, b) of
			(Pint (x), Pint (x')) => LargeInt.compare(x,x')
			| (Pstring (s1), Pstring(s2)) => String.compare(s1, s2)
			| (Ptime(s1), Ptime(s2)) => String.compare(s1, s2)
			| (Pmonth(s1), Pmonth(s2)) => String.compare(s1, s2)
			| (Pip(s1), Pip(s2)) => String.compare(s1, s2)
			| _ => Structure.compToken(a, b)
	
	structure BDSet = RedBlackSetFn(struct
                                              type ord_key = Token 
                                              val compare = compared
                                        end)
	
	(* ____ to string functions useful for debugging *)

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

	fun bdtos (d:Token):string = 
	  let fun pad x = if String.size x < 11 
		then pad (x ^ " ") else x 
	  in (pad (case d of
		PbXML(node, attrib) => "<" ^ node ^ attrib ^ ">"
	|	PeXML(node, attrib) => "</" ^ node ^ attrib ^ ">"
	|	Pint (i) => LargeInt.toString(i)
	|	Ptime(t) => t
	|	Pmonth(t) => t
	|	Pip(t)  => t
	|	Pstring(str)  => str
	|	Pwhite (str)  =>  "["^str^"]"  
	|	Other (c)  => Char.toString(c) 
	| 	Pempty => "[]" 
	|	_ => raise TyMismatch ))
	 end

	fun tokentorefine (d:Token):Refined =
		case d of
		PbXML(node, attrib) => StringConst(node ^ " + " ^ attrib) 
	|	PeXML(node, attrib) => StringConst(node ^ " + " ^ attrib) 
	|	Pint (i) => IntConst(i)
	|	Ptime(t) => StringConst(t)
	|	Pmonth(t) => StringConst(t)
	|	Pip(t)  => StringConst(t)
	|	Pstring(str)  => StringConst(str)
	|	Pwhite (str)  =>  StringConst(str)  
	|	Other(c)  =>  StringConst(Char.toString(c))  
	| 	_ => StringConst("")

	fun bdoltos (x:Token option list): string = (case x of
		h :: nil => (case h of SOME a => bdtos a | NONE => "NONE      ")
	|	h :: t => (case h of SOME a => bdtos a | NONE => "NONE       ") 
				^ "" ^ (bdoltos t)
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
		| Unique x => "Unique: " ^ (bdtos x)
		| EnumC bdset => "EnumC:\n" ^ ( implode(map bdtos (BDSet.listItems bdset),"\n") )
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
	fun some(a : 'a option) : 'a = case a of SOME x => x | NONE => raise Size
	fun isIn(ch:char,str:string):bool =
            List.exists (fn x => x = ch) (String.explode str)
	fun escapeRegex(str:string):string =
            String.translate (fn x => if isIn(x ,"^$.[]|()*+?" )
                                      then "\\" ^ (String.str x) 
                                      else String.str x) str

	fun myand(a,b) = a andalso b
	fun ltoken_equal((tk1, _):LToken, (tk2, _):LToken):bool =
	  case (tk1, tk2) of 
		    (PbXML(a,b), PbXML(a1, b1)) => (a=a1 andalso b = b1)
		  | (PeXML(a,b), PeXML(a1, b1)) =>  (a=a1 andalso b = b1) 
		  | (Ptime(a), Ptime(b)) => (a = b)
		  | (Pmonth(a), Pmonth(b)) => (a = b)
		  | (Pip(a), Pip(b)) => (a = b)
		  | (Pint(a), Pint(b)) => (a = b)
		  | (Pstring(a), Pstring(b)) => (a = b)
		  | (Pwhite(a), Pwhite(b)) => (a = b)
		  | (Other(a), Other(b)) => (a = b)
		  | (Pempty, Pempty) => true
		  (* ignoring Pgroup for now *)
		  | _ => false
	fun ltoken_ty_equal ((tk1, _):LToken, (tk2, _):LToken):bool =
	  case (tk1, tk2) of 
		    (PbXML(a,b), PbXML(a1, b1)) => true
		  | (PeXML(a,b), PeXML(a1, b1)) => true 
		  | (Ptime(a), Ptime(b)) => true
		  | (Pmonth(a), Pmonth(b)) => true
		  | (Pip(a), Pip(b)) => true
		  | (Pint(a), Pint(b)) => true
		  | (Pstring(a), Pstring(b)) => true
		  | (Pwhite(a), Pwhite(b)) => true
		  | (Other(a), Other(b)) => true
		  | (Pempty, Pempty) => true
		  (* ignoring Pgroup for now *)
		  | _ => false

	fun refine_equal (a:Refined, b:Refined):bool =
		case (a, b) of 
			(StringME(x), StringME(y)) => (x = y)
		       |(Int(x, y), Int(x1, y1)) => (x = x1 andalso y = y1)
		       |(IntConst(x), IntConst(y)) => (x = y)
		       |(StringConst(x), StringConst(y)) => (x = y)
		       |(Enum(l1), Enum(l2)) => foldr myand true 
				(ListPair.map refine_equal(l1, l2))
		       |(LabelRef(x), LabelRef(y)) => Atom.same(x, y)
		       | _ => false
	fun refine_equal_op (a:Refined option, b:Refined option):bool =
		case (a,b) of 
			(SOME a', SOME b') => refine_equal(a', b')
		|_ => false

	fun refine_equal_op1 (a:Refined option, b:Refined option):bool =
		case (a,b) of 
			(SOME a', SOME b') => refine_equal(a', b')
		| (NONE, NONE) => true
		|_ => false

    (*function to merge two AuxInfos *)
    fun mergeAux(a1, a2) =
	case (a1, a2) of 
	 ({coverage=c1, label=l1, typeComp=tc1, dataComp=dc1},
 	 {coverage=c2, label=l2, typeComp=tc2, dataComp=dc2}) =>
 	 	{coverage=c1+c2, label=l1, typeComp=tc1, dataComp=dc1}
    (*function to merge to tys that are equal structurally*)
    (*assume ty1 is before ty2*)
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
			Parray(mergeAux(a1, a2), {tokens = t1@t2, lengths = len1@len2, 
			first = mergeTy(f1, f2),
			body = mergeTy(b1, b2),
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
		| (RArray(a1, sepop1, termop1, ty1, len1), 
			RArray (a2, sepop2, termop2, ty2, len2))
			=> RArray(mergeAux(a1, a2), sepop1, termop1, mergeTy(ty1, ty2), len1) 	
		| _ => raise TyMismatch

    (* function to test of two ty's are completely equal minus the labels *)
    (* if comparetype = 0, compare everything, otherwise compare down to 
	base modulo the token list *)
    fun ty_equal (comparetype:int, ty1:Ty, ty2:Ty):bool = 
	let
		fun check_list(l1:Ty list,l2: Ty list):bool = 
		let 
			val bools = ListPair.map (fn (t1, t2) => ty_equal(comparetype, t1, t2)) (l1, l2)
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
			| (RArray(_, sepop1, termop1, ty1, len1), 
				RArray (_, sepop2, termop2, ty2, len2))
				=> refine_equal_op1(sepop1, sepop2) andalso
				   refine_equal_op1(termop1, termop2) andalso
				   ty_equal (comparetype, ty1, ty2) andalso
				   refine_equal_op1(len1, len2)
			| _ => false 
		handle Size => (print "size in ty_equal!\n" ; false)
	end
end
