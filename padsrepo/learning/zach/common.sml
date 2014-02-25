(* Defines utility functions common to all modules *)
structure Common = struct
open DataTypes (* defined in ir.sml *)
	(* ____ to string functions useful for debugging *)
	fun btos b = case b of
		IntBase => "int"
	|	LettersBase => "letters"
	|	ConstBase c => "'" ^ (String.toString c) ^ "'"
	|	REBase c => "/" ^ (String.toString c) ^"/"
	
	(*fun irtos ir = case ir of
		Base b => btos b
	|	Tuple (h::t) => "(" ^ irtos h ^ (foldr (fn(i,str) => "," ^ irtos i ^ str ) ")" t)
	|	Sum (h::t) => "[" ^ irtos h ^ (foldr (fn(i,str) => "+" ^ irtos i ^ str) "]" t)
	|	Array ir'=> "{" ^ irtos ir' ^ "}*"  
	|	Label (lb,ir) => lb ^ ":" ^ (irtos ir) *)
	
	fun indent(slist) = map (fn x => "    " ^ x) slist
	fun irtos' ir : string list = 
      case ir of 
			Base b => btos b :: nil
		|	Tuple (irlist) => "Tuple" :: indent( List.concat (map irtos' irlist) ) @ [ "End Tuple" ]
		|	Sum (irlist) => "Sum" :: indent( List.concat (map irtos' irlist) ) @ [ "End Sum" ]
		|	Array ir => "Array" :: indent (irtos' ir @ ["..."]) @ [ "End Array" ]
		|	Label (lb, ir) =>
				(case irtos' ir of
				  h :: t => (h ^" ("^lb^")") :: t
				| nil => lb :: nil)

	fun irtos ir = String.concat(map (fn x => x ^ "\n") (irtos' ir))
	
	fun bdtos (d:BaseData):string = let fun pad x = if String.size x < 11 then pad (x ^ " ") else x in (pad (case d of
		Int (s,i) => s
	|	Letters s => s
	|	Const c => c
	|	RegEx c =>  c ) )end
	fun bdtos' d =
	  case d of
	    Int (s,i) => s
	  | Letters s => s
	  | Const c => c
	  | RegEx c => c
	
	fun irdatatos' ird : string list =
	  case ird of
	    BaseD d => ("Base: '" ^ (String.toString(bdtos' d))  ^ "'") :: nil
	  | TupleD (irdlist) => "TupleD" :: indent( List.concat (map irdatatos' irdlist) ) @ [ "End TupleD" ]
	  |	SumD (ird,n) => "SumD" :: ("Branch: " ^ (Int.toString n)) :: indent( irdatatos' ird) @ [ "End SumD"]
	  |	ArrayD irdlist => "ArrayD" :: ("Num Entries: " ^ (Int.toString (length irdlist))) ::indent( List.concat (map irdatatos' irdlist))  @ [ "End ArrayD" ]

	fun irdatatos ird = String.concat(map (fn x => x ^ "\n") (irdatatos' ird))
	fun bdoltos (x:BaseData option list): string = (case x of
		h :: nil => (case h of SOME a => bdtos a | NONE => "NONE      ")
	|	h :: t => (case h of SOME a => bdtos a | NONE => "NONE       ") ^ "" ^ (bdoltos t)
	|	nil => "()\n")
	fun implode(slist, seperator) = case slist of
			h :: nil => h
		|   h :: t => h ^ seperator ^ implode(t,seperator)
		|	nil => ""
	fun ctos(c:constraint) = (case c of
		  Length x => "Length " ^ (Int.toString x)
		| Ordered x => (case x of Ascend => "Ascending" | Descend => "Decending")
		| Unique x => "Unique: " ^ (bdtos x)
		| Enum bdset => "Enum:\n" ^ ( implode(map bdtos (BDSet.listItems bdset),"\n") )
		| Range(l,h) => "Range [" ^ (Int.toString l) ^ "," ^ (Int.toString h) ^ "]"
		| Eq(((lb,i) :: idlist), c) => "Equation " ^ (foldl (fn ((lb,i),str) => str ^ " + " ^ (Rat.toString i) ^ lb) ((Rat.toString i) ^ lb) idlist) ^ " + " ^ (Rat.toString c)
		| Eq(nil,c) => "Equation " ^ (Rat.toString c)
		| Switched (lablist, branches) => "Switched " ^ (let
														val lab = implode(lablist,"\t")
														val branches' = map (fn (bdolist,bdo) => "(" ^ bdoltos bdolist ^ ") -> " ^ bdoltos [bdo]) branches
														val vals = implode(branches',"\n")
														in
														 "\n(" ^ lab ^ ")\n" ^ vals ^ "\n"
														 end)
	) ^ "\n"
	fun printConstMap cmap = Label.Map.appi (fn (lab,clist) => print (lab ^ ":\n" ^ (String.concat(map ctos clist))^ "\n")) cmap
	fun some(a : 'a option) : 'a = case a of SOME x => x | NONE => raise Size
	fun isIn(ch,str) = List.exists (fn x => x = ch) (String.explode str)
	fun escapeRegex(str) = String.translate (fn x => 	if isIn(x ,"^$.[]|()*+?" )
														then "\\" ^ (String.str x)
														else String.str x
											) str
end