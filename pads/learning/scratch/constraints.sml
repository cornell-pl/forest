(*
Zach DeVito
Kenny Zhu
Structure drives the various components of the system to find constraints such
as creating tables, and finding dependencies *)

structure Constraint = struct
open Common
structure Map = RedBlackMapFn(struct
				type ord_key = Token option
				val compare = compare
			end)
	exception Unimplemented
	exception TyMismatch

	(* defines length for the length constraint *)
	fun bdlength (based : Token) : int = case based of
		PbXML (x, y) => (String.size x) + (String.size y)
	|	PeXML (x, y) => (String.size x) + (String.size y)
	|	Ptime (x) => String.size x
	| 	Pip(x) => String.size x
	|	Pint (x) => String.size (LargeInt.toString(x))
	|	Pstring s => String.size s
	|	Pwhite c => String.size c
	|	Other c => 1
	| 	Pempty => 0
	| 	_ => 0
	
	(* the update function modifies these records when looking at constraints *)
	type constraint_record = {label: Id, constraints: (constraint * Token option) list, previous_values : BDSet.set } list
	
	(* takes a row from a base data table and the corresponding list of constraints 
	(a constraint_record) and updates the constraints using the new
	row. Returns the new constraint_record. This only looks at constraints 
	on single base data values like Unique, and Range but not Equations or Switched *)
	(* cr is a list of constraint record whose size is the number of table columns *)
	
	fun update (torow:Token option list, cr:constraint_record):constraint_record  =
		let
			val paired = ListPair.zip( torow, cr)
			fun doUpdate (bdata: Token option, 
			   {label=lab:Id,constraints, previous_values}) = 
				let
					exception InvalidConst
					val value_set = 
					  case bdata of 
					    SOME x => BDSet.add(previous_values,x)
					  | NONE => previous_values
					 (* calls update constraint on each constraint and removes it if it raises InvalidConstraint *)
					fun updateConstraints(oldConstraintlist : (constraint * Token option) list, bdataopt : Token option) : (constraint * Token option) list = 
						case oldConstraintlist of
						h :: t =>( updateConstraint(h,bdataopt) :: updateConstraints(t,bdataopt)
							handle InvalidConst => updateConstraints(t,bdataopt) )
					|	nil => nil
					 (* updates a constraint, lastbdata is the last value, bdataopt is the option of its current value *)
					and updateConstraint( (oldConst, lastbdata), bdataopt) : constraint  * Token option = 
					case bdataopt of SOME bdata =>(
						case oldConst of
						Length x => if bdlength bdata = x orelse lastbdata = NONE then Length (bdlength bdata) else  raise InvalidConst
					|	Range(a,b) => ( case bdata of
								  Pint (d) => Range(LargeInt.min(d,a),LargeInt.max(d,b))
								  |	_ => raise InvalidConst )
					|	Ordered a => (case lastbdata of SOME lastbdata => 
						  if (compared(lastbdata,bdata) = (if a = Ascend then GREATER else LESS) 
							orelse compared(lastbdata,bdata) = EQUAL) then oldConst 
						  else raise InvalidConst | NONE => oldConst)
					|	Unique x => (case lastbdata of SOME lastbdata => 
						    if compared(lastbdata,bdata) = EQUAL 
						    then Unique(bdata) 
						    else raise InvalidConst 
						  | NONE => Unique(bdata))
					   (* max number of items in EnumC is 10 *)
					|   EnumC _ => if (BDSet.numItems previous_values) > 10 
					 	      then raise InvalidConst else (EnumC(previous_values))
					|   _ => raise InvalidConst
					, SOME bdata) 
					
					|	NONE => (oldConst,lastbdata)
					
				in 
					{label=lab:Id, constraints = updateConstraints(constraints,bdata),previous_values=value_set}
				end
		in
			map doUpdate paired
		end

	(*  Takes a table and returns the partition map that represents it, and readies the 
		FunctionalDep system for running TANE *)
	fun initPartitionMap(bdolist : Token option list list, cols:Token option list list) : 
		Partition.partition FunctionalDep.ASMap.map =
		let
			val _ = Partition.init(length bdolist)
			val plists = map Partition.fromData cols
			val (map,_) = List.foldl 
			  ( fn(plist, (map,id) ) => 
			     (FunctionalDep.ASMap.insert(map, FunctionalDep.AS.singleton(id), plist),id+1) ) 
			  (FunctionalDep.ASMap.empty,0) 
			  plists
		in
			FunctionalDep.ASMap.insert(map, FunctionalDep.AS.empty, Partition.empty())
		end
	
	fun zip1(a, b)  = case a of 
					h :: t => (h,b) :: zip1(t,b)
				|	nil => nil
				
	(* map used for Switched dependencies *)
	structure BDOListMap = RedBlackMapFn(struct
									type ord_key = Token option list
									fun compare(a,b) = case (a,b) of
									  (h::t,h2::t2) => if Common.compare(h,h2) = EQUAL then compare(t,t2)
													 else Common.compare(h,h2)
									| (nil,h::t) => GREATER
									| (h::t,nil) => LESS
									| (nil,nil) => EQUAL
									end)
	fun printll ll f = app (fn x => (app (fn y => print ((f y)^" ")) x; print "\n")) ll
	
	(* determines what dependency applies to a specific mapping.
	   cols are the columns of base data options for each of the columns labeled 
	   with colLabels. values is the single column list of the value they determine. 
	   valLabel is its label. Returns the constraint that it finds  or NONE *)
	fun determine_dep ((colLabels, valLabel),(cols, values)) = 
	let
		exception DetermineFailed (* raised when a particular try fails *)
		fun toMatrix():(Id*constraint) = 
		(* finds a linear equation between integer types *)
		let
			fun strip e = case e of 
						 SOME(Pint (x) ) => x
						| _ => raise DetermineFailed (* not integers *)
			fun toRat x = Rat.reduce(x,1)
			
			(* columns, add a row of 1s so it can support a constant added to the list *)
			val cols' = List.tabulate(length (hd cols), fn _ => 1) :: map (map strip) cols
			
			val values' = map (map strip) values (*list of LargeInts *)
			val n = length cols'
			val rows = transpose(cols')
			(*val _ = print ("n: " ^ (Int.toString n) ^ " length row: " ^ (Int.toString(length rows)) ^ " length vals: " ^ (Int.toString(length values')))*)
			
			(* we need n non-trivial values for our matrix *)
			
			fun is_equal(ratlist1, ratlist2) = 
			  case (ratlist1,ratlist2) of
			    (h :: t, h2 :: t2) => if Rat.equals(h,h2) then is_equal(t,t2) else false
			  | (nil,nil) => true
			  | _ => false
			  
			fun find_n(n, (lhs,rhs) , options) = 
			if n = 0 then (lhs,rhs) else
             		  case options of
			  (r,v) :: t => 
			    let
			     val row = [map toRat r]
			     val aval = [map toRat v]
			     
			     val rowm = RatMatrix.fromList row
			     val valm = RatMatrix.fromList aval
			     
			     val lhs' = RatMatrix.appendRows(lhs,rowm)
			     val rhs' = RatMatrix.appendRows(rhs,valm)
			     
			     val is_dep = (RatMatrix.gaussJordan'(lhs'); false) handle notInvertable => true
			    in
			    	if is_dep then find_n(n, (lhs,rhs), t)
			    	else find_n(n-1, (lhs',rhs'), t)
			    end
			| nil => raise Unimplemented

			val options = ListPair.zip(rows,values')
			
			val (cm,vm) = 
			  case options of
			    (r,v) :: t =>
			    let
			      val row = [map toRat r]
			      val aval = [map toRat v]
			      val lhs = RatMatrix.fromList row
			      val rhs = RatMatrix.fromList aval
			    in
			      find_n(n-1, (lhs,rhs) , t)
			    end
			    
			  | nil => raise DetermineFailed
			
			val rest_of_rows = rows
			val rest_of_vals = values'
			
			(*val cm = RatMatrix.fromList frows
			val vm = RatMatrix.fromList fvals *)
			
			(* invert the matrix and multiply it with the value matrix to find x 
			   if we can't invert the matrix of unique values, then there is no
			   linear equation
			*)
			val inv_cm = RatMatrix.inv cm handle notInvertable => (print "INVERT\n"; print (RatMatrix.toString cm);  raise DetermineFailed)
			val x = RatMatrix.*(inv_cm,vm) 
			(*val _ = print (RatMatrix.toString x)*)
			
			val rest_of_pairs = ListPair.zip(rest_of_rows, rest_of_vals)
			(* now test the the remaining pairs fit the equation *)
			fun test_valid pairs = 
			  case pairs of 
			   (row,[aval]) :: t =>
			     let
			     	val a = RatMatrix.fromList (map (map toRat) [row])
			     	val vrat = toRat aval
			     	val mult = RatMatrix.*(a,x)
			     	val ele = RatMatrix.sub(mult,(0,0))
			     in
			     	if Rat.equals(ele,vrat) then test_valid t else false
			     end
			 | h :: t => raise Unimplemented
			 | nil => true
			val is_valid = test_valid rest_of_pairs
			(* construct the Eq constraint *)
			val clist = map hd (RatMatrix.toList x)
			val (const,clist) = (hd clist, tl clist)
		in
			if is_valid then (valLabel, Eq( ListPair.zip(colLabels,clist), const ) )
			else raise DetermineFailed
		end handle Empty => raise DetermineFailed | Subscript => 
				raise DetermineFailed
		(* find a mapping between values of one type and values of another *)
		fun toSwitched(): Id*constraint =
		let
			val rows = transpose cols
			val vals = map hd ( values)
			val smap = foldr (fn((row,v), theMap) => BDOListMap.insert(theMap,row,v) ) BDOListMap.empty (ListPair.zip(rows,vals))	
			val slist = BDOListMap.foldri (fn(row,v,list) => (row,v) :: list) 
						nil smap
			
			
			(* finds the most popular result of the switch and 
			   makes it a default value to try to reduce switched size *)
			fun tryDefault() = 
			let
				fun insert(m,v,r) = case Map.find(m,v) of 
				  SOME x => Map.insert(m,v,x+1)
				| NONE => Map.insert(m,v,1)
				val counts = foldr (fn ((row,v),m) => insert(m,v,row) ) Map.empty slist
				val (v,count) = Map.foldri (fn (key,c,(mkey,max)) => if c > max then (key,c) else (mkey,max)) (NONE,0) counts
				val filteredlist = List.filter (fn (row,v') =>not(compare(v,v') = EQUAL)) slist
				val filteredlist = ([SOME (Pstring "*")], v) :: filteredlist 
			in
				if length filteredlist < length rows div 4 andalso length filteredlist < 50 then
					(valLabel,Switched(colLabels,filteredlist))
				else
					raise DetermineFailed
			end
		in
			if BDOListMap.numItems smap < length rows div 4 andalso BDOListMap.numItems smap < 50 then
				(valLabel,Switched(colLabels,slist))
			else
				tryDefault() (* see if the default makes it small enough *)
		end handle Empty => raise DetermineFailed
		
		fun tryEach flist =
		  case flist of
		    h :: t => ((SOME (h ())) handle DetermineFailed => tryEach t)
		  | nil => NONE
	in
		tryEach [ toMatrix, toSwitched]
	end
	
	(* takes a list of columns and a dependency in number form
	   and extracts the correct columns to send to determineDeps *)
	(* can also be used to map a list of headers?? *)
	fun dep_cols bdocols (alist : int list, a : int)  = 
	let
		val cols = map (fn x => List.nth(bdocols,x)) alist
		val vals = List.nth(bdocols,a)
	in
		(cols,vals)
	end

	(* utility functions to determine if a dependency is "useless" *)
	(*determine if a label is found inside Ty *)
	fun is_inside (lab:Id) (ty:Ty) = case ty of
            Base ({coverage, label}, _) => 
		(case label of 
			SOME id => (if Atom.same(lab, id) then true else false)
		        | NONE => false)
	  | TBD _ => false
	  | Bottom _ => false
	  | Pstruct (_, tylist) => List.exists (is_inside lab) tylist
	  | Punion (_, tylist) => List.exists (is_inside lab) tylist
	  | Parray (_, arr) => is_inside lab (#first arr) orelse 
				is_inside lab (#body arr) orelse 
				is_inside lab (#last arr)
	  | _ => false

	fun try f list = case list of 
		h :: t => (f(h)  handle _ => try f t)
	 | nil => raise Empty 
	
	(* finds the Punion with a particular label in Ty *)
	fun sum_with_lab (lab:Id) (ty:Ty) = case ty of 
		Punion({coverage, label}, tylist) => 
				(case label of SOME id =>
					(if Atom.same(id, lab) then ty
				 	else try (sum_with_lab lab) tylist)
				     | NONE => try (sum_with_lab lab) tylist)
		| Base _ => raise Empty
		| TBD _ => raise Empty
		| Pstruct (_, tylist) => try (sum_with_lab lab) tylist
		| Parray (_, {tokens, lengths, first, body, last}) => 
			try (sum_with_lab lab) [first, body, last]
		| _ => raise TyMismatch 

	(* determines if a dependency is useless *)
	fun is_useless ty (labList: Id list, determined:Id) =
	let
		(* a sum determines a value inside itself *)
		fun inside_sum() =
		let
			fun is_fun lab = let
				val sumty = sum_with_lab lab ty
			in
				is_inside determined sumty
			end handle Empty => false
		in
			case labList of 
			  [h] => is_fun h
			|  _ => false
		end 
		
		(* a value inside a sum determines the sum *)
		fun determines_from_inside() = 
		let
			val sumty = sum_with_lab determined ty
		in
			List.exists (fn x => is_inside x sumty) labList 
		end handle Empty => false
	in
		List.exists (fn x => x()) [ inside_sum, determines_from_inside ]
	end

(* the main constraining function. Takes an unlabeled IR and data
parsed using that IR, and returns:
the labeled IR, the constraint_map that constraints it, and the labels that were used.
Also prints out information about the dependencies and keys it found *)
	fun constrain'(ty:Ty) = 
	let
		(* make the table *)
		fun getnumrecords (ty) = 
			case ty of Base(aux, _) => (#coverage aux)
			| TBD (aux, _, _) => (#coverage aux)
			| Bottom (aux, _, _) => (#coverage aux)
			| Pstruct (aux, _) => (#coverage aux)
			| Punion (aux, _) => (#coverage aux)
			| Parray (aux, _) => (#coverage aux)
			| _ => raise TyMismatch 

		val tytable = Table.genTable (getnumrecords(ty)) ty
(*
		val _ = print ("Number of records: "^ Int.toString(getnumrecords(ty)) ^"\n")
		val _ = if Options.print_tables then 
			Table.printTable(tytable) else ()
*)
		val header = #1 tytable
		val bdocols = #2 tytable
		val bdolist = transpose(bdocols)
		val _ = if Options.print_tables then 
			Table.printTable(header, bdolist) else ()

		(* take a table entry and run TANE on it, find the constraints that apply *)
		val partitions = initPartitionMap(bdolist, bdocols)
		val (deps,keys) = FunctionalDep.tane(partitions)
		(* val _ = print (String.concat (map bdoltos bdolist) ) *)
			
		(* finds the labels associated with a numbered dep *)
		(*returns Id list * Id *)
		fun depToLabel( alist : int list, a : int):Id list*Id = 
			(map (fn b => List.nth(header,b) ) alist, List.nth(header,a) )
		(* labeled_deps is (Id list * Id) list *)
		val labeled_deps = map depToLabel deps
			
		(* filter out the useless deps *)
		val (labeled_deps:(Id list * Id) list, deps) = 
			ListPair.unzip( List.filter (fn (x,_) => not (is_useless  ty x)) 
					(ListPair.zip(labeled_deps,deps)))
			
		(* label and print the deps and keys *)
(*
		val _ = print ("num of dependencies: " ^ Int.toString(length(deps))^ "\n")
		val _ = print ("num of keys: " ^ Int.toString(length keys) ^ "\n")
*)
		val labeled_keys = map (map (fn x => List.nth(header,x))) keys
		fun printDep ( llist, l ) = print ("{" ^ 
			(concat (map (fn x => x ^ ",") (idstostrs llist))) ^ 
			"} -> " ^ Atom.toString(l) ^ "\n")
		    
	        val _ = if Options.print_functional_deps then print "Dependencies:\n" else ()
	        val _ = if Options.print_functional_deps 
	              then app printDep labeled_deps
	              else ()
		val _ = if Options.print_functional_deps 
		         then app (fn l => 
				print ("Key {" ^ implode(idstostrs(l),",")  ^ "}\n") 
				) labeled_keys
		         else ()
		
		(* determine what constraints apply *)
		val dpl = map (dep_cols bdocols) deps
		val labl:(Id list * Id) list = map (dep_cols header) deps
		val dpl = map (fn (y,x) => (y,map (fn z => [z]) x)) dpl
		val found_deps: (Id*constraint) option list = 
			map determine_dep (ListPair.zip(labl,dpl) )
		val found_deps = List.filter (fn x => case x of 
			SOME _ => true | NONE => false) found_deps
		val found_deps:(Id* constraint) list = map some found_deps

 		(* find the single column constraints *)
		val a = (Int.toLarge(some(Int.maxInt)), Int.toLarge(some(Int.minInt)))
		(* initialize each entry to some starting values *)
		val consts = zip1([ Range a, Length 0, Ordered Ascend, 
				    Ordered Descend, Unique(Pempty), 
				    EnumC BDSet.empty ], NONE)
		(*depstart is a list of constraints of size num of cols in the table*)
		val depstart:constraint_record = map (fn x=> {label = x, 
			constraints = consts:(constraint*Token option) list, 
			previous_values= BDSet.empty}) header
		val consts:constraint_record = foldr update depstart bdolist (* returns a constraint_record *)
			
		(* combine the constraints found through dep analysis with the single column constraints *)
		fun add_to_consts consts (lab, dep) = 
		  map (fn {label,constraints, previous_values} => 
		    if Atom.same(label, lab) 
		      then {label=label, constraints=(dep,NONE)::constraints, previous_values=previous_values} 
		      else {label=label, constraints=constraints, previous_values=previous_values}
		     ) consts
		val consts = foldr (fn (dep, c) => add_to_consts c dep) consts found_deps
	in foldr (fn ({label,constraints,previous_values},cm) => 
			LabelMap.insert(cm,label,map #1 constraints)) LabelMap.empty consts
	end
end
