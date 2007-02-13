(* Defines utility functions common to all modules *)
structure Common = struct
	open Types (* defined in types.sml *)

	fun idcompare (id1, id2) = String.compare(Atom.toString(id1), Atom.toString(id2))

	structure LabelMap = SplayMapFn(struct
                 type ord_key = Id
                 val compare = idcompare
        end)
	structure LabelSet = SplaySetFn(struct
                 type ord_key = Id
                 val compare = idcompare
        end)
	exception TyMismatch

	(* defines an arbitrary order on tokens
	to put it in maps *)
	(* it overrides some of the ordering in structure.sml *)
	fun compare(a,b) = case (a,b) of
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
                        | _                        => raise TyMismatch
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
	| Enum of BDSet.set (* set of values it takes on *)
	and ordered = Ascend | Descend

	fun bdtos (d:Token):string = 
	  let fun pad x = if String.size x < 11 
		then pad (x ^ " ") else x 
	  in (pad (case d of
		PbXML(node, attrib) => node ^ " + " ^ attrib 
	|	PeXML(node, attrib) => node ^ " + " ^ attrib 
	|	Pint (i) => LargeInt.toString(i)
	|	Ptime(t) => t
	|	Pmonth(t) => t
	|	Pip(t)  => t
	|	Pstring(str)  => str
	|	Pwhite (str)  =>  str  
	| 	_ => "" ))
	 end

	fun bdoltos (x:Token option list): string = (case x of
		h :: nil => (case h of SOME a => bdtos a | NONE => "NONE      ")
	|	h :: t => (case h of SOME a => bdtos a | NONE => "NONE       ") ^ "" ^ (bdoltos t)
	|	nil => "()\n")

	fun idstostrs(idlist: Id list) = map (fn id => Atom.toString(id)) idlist
		
	(* link a list of labels with separators *)
	fun implode(slist, seperator) = case slist of
			h :: nil => h
		|   h :: t => h ^ seperator ^ implode(t,seperator)
		|	nil => ""
	fun ctos(c:constraint) = (case c of
		  Length x => "Length " ^ (Int.toString x)
		| Ordered x => (case x of Ascend => "Ascending" | Descend => "Decending")
		| Unique x => "Unique: " ^ (bdtos x)
		| Enum bdset => "Enum:\n" ^ ( implode(map bdtos (BDSet.listItems bdset),"\n") )
		| Range(l,h) => "Range [" ^ (LargeInt.toString l) ^ "," ^ 
				(LargeInt.toString h) ^ "]"
		| Eq(((lb,i) :: idlist), c) => "Equation " ^ (foldl (fn ((lb,i),str) => str ^ " + " ^ (Rat.toString i) ^ Atom.toString(lb)) ((Rat.toString i) ^ Atom.toString(lb)) idlist) ^ " + " ^ (Rat.toString c)
		| Eq(nil,c) => "Equation " ^ (Rat.toString c)
		| Switched (lablist, branches) => "Switched " ^ (let
														val lab = implode(idstostrs(lablist),"\t")
														val branches' = map (fn (bdolist,bdo) => "(" ^ bdoltos bdolist ^ ") -> " ^ bdoltos [bdo]) branches
														val vals = implode(branches',"\n")
														in
														 "\n(" ^ lab ^ ")\n" ^ vals ^ "\n"
														 end)
	) ^ "\n"
	fun printConstMap cmap = LabelMap.appi (fn (lab,clist) => print (Atom.toString(lab) ^ ":\n" ^ (String.concat(map ctos clist))^ "\n")) cmap
	fun some(a : 'a option) : 'a = case a of SOME x => x | NONE => raise Size
	fun isIn(ch,str) = List.exists (fn x => x = ch) (String.explode str)
	fun escapeRegex(str) = String.translate (fn x => 	if isIn(x ,"^$.[]|()*+?" )
														then "\\" ^ (String.str x)
														else String.str x
											) str
end
