(*
Zach DeVito
Structure drives the various components of the system to find constraints such
as creating tables, and finding dependencies *)

structure Constraint = struct
open Common
structure Map = RedBlackMapFn(struct
							type ord_key = BaseData option
							val compare = compare
							
							end)
	exception Unimplemented
	exception IRMismatch
	
	(* defines length for the length constraint *)
	fun bdlength (based : BaseData) : int = case based of
		Int (x,_) => String.size x
	|	Letters s => String.size s
	|	Const c => String.size c
	|	RegEx s => String.size s
	
	(* the update function modifies these records when looking at constraints *)
	type constraint_record = {label: Label.id, constraints: (constraint * BaseData option) list, previous_values : BDSet.set } list
	
	(* takes a row from a base data table and the corresponding list of constraints (a constraint_record) and updates the constraints using the new
	row. Returns the new constraint_record. This only looks at constraints on single base data values like Unique, and Range but not Equations or
	Switched *)
	
	fun update (bdatalist: BaseData option list, cr :  constraint_record ) :  constraint_record  =
		let
			val paired = ListPair.zip( bdatalist, cr)
			fun doUpdate (bdata: BaseData option, {label,constraints, previous_values}) = 
					let
						exception InvalidConst
						val value_set = 
						  case bdata of 
						    SOME x => BDSet.add(previous_values,x)
						  | NONE => previous_values
						 (* calls update constraint on each constraint and removes it if it raises InvalidConstraint *)
						fun updateConstraints(oldConstraintlist : (constraint * BaseData option) list, bdataopt : BaseData option) : (constraint * BaseData option) list = 
							case oldConstraintlist of
							h :: t =>( updateConstraint(h,bdataopt) :: updateConstraints(t,bdataopt)
									  handle InvalidConst => updateConstraints(t,bdataopt) )
						|	nil => nil
						 (* updates a constraint, lastbdata is the last value, bdataopt is the option of its current value *)
						and updateConstraint( (oldConst, lastbdata), bdataopt) : constraint  * BaseData option = 
							case bdataopt of SOME bdata =>(
							case oldConst of
							Length x => if bdlength bdata = x orelse lastbdata = NONE then Length (bdlength bdata) else  raise InvalidConst
						|	Range(a,b) => ( case bdata of
												Int (_,d) => Range(Int.min(d,a),Int.max(d,b))
											|	_ => raise InvalidConst )
						|	Ordered a => (case lastbdata of SOME lastbdata => if (compared(lastbdata,bdata) = (if a = Ascend then GREATER else LESS) orelse compared(lastbdata,bdata) = EQUAL) then 
													oldConst else raise InvalidConst | NONE => oldConst)
						|	Unique x => (case lastbdata of SOME lastbdata => if compared(lastbdata,bdata) = EQUAL then Unique(bdata) else raise InvalidConst | NONE => Unique(bdata))
						|   Enum _ => if (BDSet.numItems previous_values) > 10 then raise InvalidConst else (Enum(previous_values))
						|   _ => raise InvalidConst
						,SOME bdata) 
						
						|	NONE => (oldConst,lastbdata)
						
					in 
						{label = label, constraints = updateConstraints(constraints,bdata),previous_values=value_set}
					end
		in
			map doUpdate paired
		end

	(*converts rows into columns. Must contain at least one element *)
	fun swaplists( alistlist : 'a list list) : 'a list list = 
		let
			fun addtohead(alist, alistlist) = case (alist,alistlist) of
			  (h::t, h2::t2) => (h :: h2) :: addtohead(t,t2)
			| (nil,nil) => nil	
			| _			   => raise IRMismatch
		in
			List.foldr addtohead (List.map (fn x => nil) (List.hd alistlist)) alistlist
		end
	(*  Takes a table and returns the partition map that represents it, and readies the 
		FunctionalDep system for running TANE *)
	fun initPartitionMap(bdolist : BaseData option list list) : Partition.partition FunctionalDep.ASMap.map =
		let
			val _ = Partition.init(length bdolist)
			val cols = swaplists(bdolist)
			
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
									type ord_key = BaseData option list
									fun compare(a,b) = case (a,b) of
									  (h::t,h2::t2) => if Common.compare(h,h2) = EQUAL then compare(t,t2)
													 else Common.compare(h,h2)
									| (nil,h::t) => GREATER
									| (h::t,nil) => LESS
									| (nil,nil) => EQUAL
									end)
	fun printll ll f = app (fn x => (app (fn y => print ((f y)^" ")) x; print "\n")) ll
	
	(* determines what dependency applies to a specific mapping.
	   cols are the columns of base data options for each of the columns labeld with colLabels.
	   values is the single column list of the value they determine. valLabel is its label.
	   Returns the constraint that it finds  or NONE *)
	fun determine_dep ((colLabels, valLabels),(cols, values)) = 
	let
		exception DetermineFailed (* raised when a particular try fails *)
		fun toMatrix() = 
		(* finds a linear equation between integer types *)
		let
			fun strip e = case e of 
						 SOME( Int (_,x) ) => x
						| _ => raise DetermineFailed (* not integers *)
			fun toRat x = Rat.fromInts(x,1)
			
			(* columns, add a row of 1s so it can support a constant added to the list *)
			val cols' = List.tabulate(length (hd cols), fn _ => 1) :: map (map strip) cols
			
			val values' = map (map strip) values
			val n = length cols'
			val rows = swaplists(cols')
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
			if is_valid then (valLabels, Eq( ListPair.zip(colLabels,clist), const ) )
			else raise DetermineFailed
		end handle Empty => raise DetermineFailed | Subscript => raise DetermineFailed
		(* find a mapping between values of one type and values of another *)
		fun toSwitched() =
		let

			val rows = swaplists cols
			val vals = map hd ( values)
			val smap = foldr (fn( (row,v) ,theMap) => BDOListMap.insert(theMap,row,v) ) BDOListMap.empty (ListPair.zip(rows,vals))	
			val slist = BDOListMap.foldri ( fn(row,v,list) => (row,v) :: list ) nil smap
			
			
			(* finds the most popular result of the switch and makes it a default value
			   to try to reduce switched size *)
			fun tryDefault() = 
			let
				fun insert(m,v,r) = case Map.find(m,v) of 
									  SOME x => Map.insert(m,v,x+1)
									| NONE => Map.insert(m,v,1)
				val counts = foldr (fn ((row,v),m) => insert(m,v,row) ) Map.empty slist
				val (v,count) = Map.foldri (fn (key,c,(mkey,max)) => if c > max then (key,c) else (mkey,max)) (NONE,0) counts
				val filteredlist = List.filter (fn (row,v') =>not(compare(v,v') = EQUAL)) slist
				val filteredlist = ([SOME (Letters "*")], v) :: filteredlist 
			in
				if length filteredlist < length rows div 4 andalso length filteredlist < 50 then
					(valLabels,Switched(colLabels,filteredlist))
				else
					raise DetermineFailed
			end
		in
			if BDOListMap.numItems smap < length rows div 4 andalso BDOListMap.numItems smap < 50 then
				(valLabels,Switched(colLabels,slist))
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
	fun dep_cols bdocols (alist : int list,a : int)  = 
	let
		val cols = map (fn x => List.nth(bdocols,x)) alist
		val vals = List.nth(bdocols,a)
	in
		(cols,vals)
	end

	(* utility functions to determine if a dependency is "useless" *)
	(*determine if a label is found inside IR *)
	fun is_inside lab ir = case ir of
		Base _ => false
	  | Tuple irlist => List.exists (is_inside lab) irlist
	  | Sum irlist => List.exists (is_inside lab) irlist
	  | Array ir''=> is_inside lab ir''
	  | Label(l,ir') => if lab = l then true else is_inside lab ir'
	fun isSum(ir) = case ir of Sum _ => true | _ => false
	
	fun try f list = case list of 
		h :: t => (f(h)  handle _ => try f t )
	 | nil => raise Empty 
	
	(* finds the sum with a particular label in IR *)
	fun sum_with_lab lab ir = case ir of 
		Label(l,ir') => (if l = lab andalso isSum(ir') then ir'
								else case ir' of
								  Base _ => raise Empty
								| Tuple irlist => try (sum_with_lab lab) irlist
								| Sum irlist => try (sum_with_lab lab) irlist
								| Array ir''=> sum_with_lab lab ir''
								| _ => raise IRMismatch )
		| _ => raise IRMismatch
	(* determines if a dependency is useless *)
	fun is_useless lab_ir (labList, determined) =
	let
		(* a sum determines a value inside itself *)
		fun inside_sum() =
		let
			fun is_fun lab = let
				val ir = sum_with_lab lab lab_ir
			in
				is_inside determined ir
			end handle Empty => false
		in
			case labList of 
			  [h] => is_fun h
			|  _ => false
		end 
		
		(* a value inside a sum determines the sum *)
		fun determines_from_inside() = 
		let
			val ir' = sum_with_lab determined lab_ir
		in
			List.exists (fn x => is_inside x ir') labList 
		end handle Empty => false
	in
		List.exists (fn x => x()) [ inside_sum, determines_from_inside ]
	end

(* the main constraining function. Takes an unlabeled IR and data
parsed using that IR, and returns:
the labeled IR, the constraint_map that constraints it, and the labels that were used.
Also prints out information about the dependencies and keys it found *)
	fun constrain'(ir : IR, irdata : IRData ) = 
	let
		(* make the tables *)
		val (labIR,tables,usedLabels) = Table.genTables(ir, irdata)
		val _ = if Options.print_tables then Table.printTableMap(tables) else ()

		(* take a table entry and run TANE on it, find the constraints that apply *)
		fun constrainTable( (header, bdolist) : Table.table_map_entry ) =
		let
			val partitions = initPartitionMap(bdolist)
			
			val bdocols = swaplists(bdolist)
			
			val (deps,keys) = FunctionalDep.tane(partitions)
			
			(* val _ = print (String.concat (map bdoltos bdolist) ) *)
			
			(* finds the labels associated with a numbered dep *)
			fun depToLabel( alist : int list, a : int) = (map (fn b => List.nth(header,b) ) alist, List.nth(header,a) )
			val labeled_deps = map depToLabel deps
			
			(* filter out the useless deps *)
			val (labeled_deps,deps) = ListPair.unzip( List.filter (fn (x,_) => not (is_useless  labIR x)) (ListPair.zip(labeled_deps,deps)))
			
			(* label and print the deps and keys *)
			val labeled_keys = map (map (fn x => List.nth(header,x))) keys
			fun printDep ( llist, l ) = print ("{" ^ (concat (map (fn x => x ^ ",") llist)) ^ "} -> " ^ l ^ "\n")
		    
		    val _ = if Options.print_functional_deps then print "Dependencies:\n" else ()
		    val _ = if Options.print_functional_deps 
		              then app printDep labeled_deps
		              else ()
			val _ = if Options.print_functional_deps 
			         then app (fn list => print ("Key {" ^ implode(list,",")  ^ "}\n") )labeled_keys
			         else ()
			
			(* determine what constraints apply *)
			val dpl = map (dep_cols bdocols) deps
			val labl = map (dep_cols header) deps
			val dpl = map (fn (y,x) => (y,map (fn z => [z]) x)) dpl
			val found_deps = map determine_dep (ListPair.zip(labl,dpl) )
			val found_deps = List.filter (fn x => case x of SOME _ => true | NONE => false) found_deps
			val found_deps = map some found_deps
 			
 			(* find the single column constraints *)
			val a = (some(Int.maxInt),some(Int.minInt))
			(* initialize each entry to some starting values *)
			val consts = zip1([ Range a, Length 0, Ordered Ascend, Ordered Descend, Unique(Const "NONE"), Enum BDSet.empty ],NONE)
			val depstart = map (fn x=> {label = x, constraints = consts, previous_values= BDSet.empty}) header
			val consts = foldr update depstart bdolist
			
			(* combine the constraints found through dep analysis with the single column constraints *)
			fun add_to_consts consts (lab,dep) = 
			  map (fn {label,constraints,previous_values} => 
			    if label = lab 
			      then {label=label,constraints=(dep,NONE)::constraints,previous_values=previous_values} 
			      else {label=label,constraints=constraints,previous_values=previous_values}
			     ) consts
			val consts = foldr (fn (x,c) => add_to_consts c x) consts found_deps
			(*
			fun ctos' (c,junk) = "\n " ^ ctos c ^ ","
			val const_strings = map (fn {label, constraints} => case constraints of h :: t => label ^ (String.concat (map ctos' constraints)) ^ "\n"
														  | nil => "") consts
			val _ = app print const_strings *)
			val const_map = foldr (fn ({label,constraints,previous_values},cm) => Label.Map.insert(cm,label,map #1 constraints)) Label.Map.empty consts
		in
			const_map
		end
			
		val _ = if Options.print_pre_reduced_IR then print (irtos labIR) else ()
		(* run analysis on every table in the table map, then combine the resulting constraints *)
		val union = Label.Map.unionWith (fn (x,y) => raise IRMismatch) (* we should have no collisions in this union *)
		val const_map = Table.LMap.foldr (fn (tab,cm) => union(cm,(constrainTable tab))) Label.Map.empty tables 
		val used_const_map = Table.LMap.filteri (fn (lb,e) => Label.Set.member(usedLabels,lb) ) const_map
		(* val _ = printConstMap used_const_map *)
		(* val _ = Label.Set.app (fn x => print (x ^ "\n")) usedLabels *)
	in
		(labIR,used_const_map,usedLabels)
	end
end
