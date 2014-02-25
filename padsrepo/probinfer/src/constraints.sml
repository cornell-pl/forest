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
structure BDOSet = RedBlackSetFn(struct
				type ord_key = Token option
				val compare = compare
			end)

structure NewMap = RedBlackMapFn(struct
				type ord_key = BSToken option
				val compare = newcompare
			end)
structure NewBDOSet = RedBlackSetFn(struct
				type ord_key = BSToken option
				val compare = newcompare
			end)

	exception Unimplemented
	exception TyMismatch

	(* defines length for the length constraint *)
	fun bdlength (based : Token) : int = case based of
		PbXML (x, y) => (String.size x) + (String.size y)
	|	PeXML (x, y) => (String.size x) + (String.size y)
	|	Ptime (x) => String.size x
	|	Pdate (x) => String.size x
	| 	Pip(x) => String.size x
	| 	Phostname(x) => String.size x
	| 	Ppath(x) => String.size x
	| 	Purl(x) => String.size x
	| 	Pemail(x) => String.size x
	| 	Pmac(x) => String.size x
	|	Pint (x, s) => String.size s
	|	Pfloat (a, b) => String.size (a) + String.size (b)
	|	Pstring s => String.size s
	|	Pwhite c => String.size c
	|	Other c => 1
	| 	Pempty => 0
	| 	_ => 0

	(* the update function modifies these records when looking at constraints *)
	type constraint_record = {label: Id, constraints: (constraint * Token option) list, previous_values : BDSet.set } list

	type newconstraint_record = {label: Id, constraints: (newconstraint * BSToken option) list, previous_values : NewBDSet.set } list
	
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
					fun updateConstraints(oldConstraintlist:(constraint * Token option) list, bdataopt:Token option) : (constraint * Token option) list = 
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
								  Pint (d, _) => Range(LargeInt.min(d,a),LargeInt.max(d,b))
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
					 	      then raise InvalidConst else 
								EnumC(value_set)
					|   _ => raise InvalidConst
					, SOME bdata) 
					
					|	NONE => (oldConst,lastbdata)
					
				in 
					{label=lab:Id, constraints = updateConstraints(constraints,bdata),previous_values=value_set}
				end
		in
			map doUpdate paired
		end

	fun newupdate (torow:BSToken option list, cr:newconstraint_record):newconstraint_record  =
		let
			val paired = ListPair.zip( torow, cr)
			fun doUpdate (bdata: BSToken option, 
				     {label=lab:Id,constraints, previous_values}) = 
				let
					exception InvalidConst
					val value_set = 
					  case bdata of 
					    SOME x => NewBDSet.add(previous_values,x)
					  | NONE => previous_values
					 (* calls update constraint on each constraint and removes it if it raises InvalidConstraint *)
					fun updateConstraints(oldConstraintlist:(newconstraint * BSToken option) list, bdataopt:BSToken option) : (newconstraint * BSToken option) list = 
						case oldConstraintlist of
						h :: t =>( updateConstraint(h,bdataopt) :: updateConstraints(t,bdataopt)
							handle InvalidConst => updateConstraints(t,bdataopt) )
					|	nil => nil
					 (* updates a constraint, lastbdata is the last value, bdataopt is the option of its current value *)
					and updateConstraint( (oldConst, lastbdata), bdataopt) : newconstraint  * BSToken option = 
					case bdataopt of SOME bdata =>(
						case oldConst of
						NLength x => if btokenLength bdata = x orelse lastbdata = NONE then NLength (btokenLength bdata) else  raise InvalidConst
					|	NRange(a,b) => ( case bdata of
								  (PPint, d) => NRange(LargeInt.min(Option.valOf(LargeInt.fromString d),a),LargeInt.max(Option.valOf(LargeInt.fromString d),b))
								  |	_ => raise InvalidConst )
					|	NOrdered a => (case lastbdata of SOME lastbdata => 
						  if (newcompared(lastbdata,bdata) = (if a = NAscend then GREATER else LESS) 
							orelse newcompared(lastbdata,bdata) = EQUAL) then oldConst 
						  else raise InvalidConst | NONE => oldConst)
					|	NUnique x => (case lastbdata of SOME lastbdata => 
						    if newcompared(lastbdata,bdata) = EQUAL 
						    then NUnique(bdata) 
						    else raise InvalidConst 
						  | NONE => NUnique(bdata))
					   (* max number of items in EnumC is 10 *)
					|   NEnumC _ => if (NewBDSet.numItems previous_values) > 10 
					 	      then raise InvalidConst else 
								NEnumC(value_set)
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

	fun newzip1(a, b)  = case a of 
					h :: t => (h,b) :: newzip1(t,b)
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

	structure NewBDOListMap = RedBlackMapFn(struct
				type ord_key = BSToken option list
				fun compare(a,b) = case (a,b) of
				  (h::t,h2::t2) => if Common.newcompare(h,h2) = EQUAL then compare(t,t2)
						 else Common.newcompare(h,h2)
				| (nil,h::t) => GREATER
				| (h::t,nil) => LESS
				| (nil,nil) => EQUAL
									end)

	fun printll ll f = app (fn x => (app (fn y => print ((f y)^" ")) x; print "\n")) ll
	
	fun printDep ( llist, l ) = print ("{" ^ 
			(concat (map (fn x => x ^ ",") (idstostrs llist))) ^ 
			"} -> " ^ Atom.toString(l) ^ "\n")
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
						 SOME(Pint (x, _) ) => x
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
			val smap = foldr (fn((row,v), theMap) => 
					case (row, v) of 
						([NONE], _) => theMap
						| (_, NONE) => theMap
						| _ => (BDOListMap.insert(theMap,row,v)) ) 
					BDOListMap.empty (ListPair.zip(rows,vals))	
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
				if length filteredlist < length rows div 2 andalso length filteredlist < 50 
					andalso length filteredlist>1 then
					(valLabel, Switched(colLabels,filteredlist))
				else
					raise DetermineFailed
			end
		in
			if BDOListMap.numItems smap < length rows div 4 andalso BDOListMap.numItems smap < 50 
				andalso length slist > 1 then
				(valLabel,Switched(colLabels,slist))
			else
				tryDefault() (* see if the default makes it small enough *)
		end handle Empty => raise DetermineFailed | Subscript => 
				raise DetermineFailed
		(* find a mapping between values of one type and values of another *)
		fun toSwitched(): Id*constraint =
		let
			val rows = transpose cols
			val vals = map hd ( values)
			val smap = foldr (fn((row,v), theMap) => 
					case (row, v) of 
						([NONE], _) => theMap
						| (_, NONE) => theMap
						| _ => (BDOListMap.insert(theMap,row,v)) ) 
					BDOListMap.empty (ListPair.zip(rows,vals))	
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
				if length filteredlist < length rows div 2 andalso length filteredlist < 50 
					andalso length filteredlist>1 then
					(valLabel, Switched(colLabels,filteredlist))
				else
					raise DetermineFailed
			end
		in
			if BDOListMap.numItems smap < length rows div 4 andalso BDOListMap.numItems smap < 50 
				andalso length slist > 1 then
				(valLabel,Switched(colLabels,slist))
			else
				tryDefault() (* see if the default makes it small enough *)
		end handle Empty => raise DetermineFailed
		
		fun tryEach flist =
		  case flist of
		    h :: t => ((SOME (h ())) handle DetermineFailed => tryEach t)
		  | nil => NONE
	in
		tryEach [toSwitched] (*, toMatrix]*)
	end

	fun newdetermine_dep ((colLabels, valLabel),(cols, values)) = 
	let
		exception DetermineFailed (* raised when a particular try fails *)
		fun toMatrix():(Id*constraint) = 
		(* finds a linear equation between integer types *)
		let
			fun strip e = case e of 
						 SOME((PPint, x) ) => Option.valOf(LargeInt.fromString x)
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
		end handle Empty => raise DetermineFailed| Subscript => 
				raise DetermineFailed
		(* find a mapping between values of one type and values of another *)
		fun toSwitched(): Id*newconstraint =
		let
			val rows = transpose cols
			val vals = map hd ( values)
			val smap = foldr (fn((row,v), theMap) => 
					case (row, v) of 
						([NONE], _) => theMap
						| (_, NONE) => theMap
						| _ => (NewBDOListMap.insert(theMap,row,v)) ) 
					NewBDOListMap.empty (ListPair.zip(rows,vals))	
			val slist = NewBDOListMap.foldri (fn(row,v,list) => (row,v) :: list) 
						nil smap
			
			(* finds the most popular result of the switch and 
			   makes it a default value to try to reduce switched size *)
			fun tryDefault() = 
			let
				fun insert(m,v,r) = case NewMap.find(m,v) of 
				  SOME x => NewMap.insert(m,v,x+1)
				| NONE => NewMap.insert(m,v,1)
				val counts = foldr (fn ((row,v),m) => insert(m,v,row) ) NewMap.empty slist
				val (v,count) = NewMap.foldri (fn (key,c,(mkey,max)) => if c > max then (key,c) else (mkey,max)) (NONE,0) counts
				val filteredlist = List.filter (fn (row,v') =>not(newcompare(v,v') = EQUAL)) slist
				val filteredlist = ([SOME (PPblob, "*")], v) :: filteredlist   (* what's this PPblob? *) 
			in
				if length filteredlist < length rows div 2 andalso length filteredlist < 50 
					andalso length filteredlist>1 then
					(valLabel, NSwitched(colLabels,filteredlist))
				else
					raise DetermineFailed
			end
		in
			if NewBDOListMap.numItems smap < length rows div 4 andalso NewBDOListMap.numItems smap < 50 
				andalso length slist > 1 then
				(valLabel,NSwitched(colLabels,slist))
			else
				tryDefault() (* see if the default makes it small enough *)
		end handle Empty => raise DetermineFailed
		
		fun tryEach flist =
		  case flist of
		    h :: t => ((SOME (h ())) handle DetermineFailed => tryEach t)
		  | nil => NONE
	in
		tryEach [toSwitched] (*, toMatrix]*)
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
            Base ({coverage, label, ...}, _) => 
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
	  | RArray (_, _, _, body, _, _) => is_inside lab body
	  | Poption (_, body) => is_inside lab body
	  | _ => false

	fun newis_inside (lab:Id) (ty:NewTy) = case ty of
            PPBase ({coverage, label, ...}, _) => 
		(case label of 
			SOME id => (if Atom.same(lab, id) then true else false)
		        | NONE => false)
	  | PPTBD _ => false
	  | PPBottom _ => false
	  | PPstruct (_, tylist) => List.exists (newis_inside lab) tylist
	  | PPunion (_, tylist) => List.exists (newis_inside lab) tylist
	  | PParray (_, arr) => newis_inside lab (#first arr) orelse 
				newis_inside lab (#body arr) orelse 
				newis_inside lab (#last arr)
	  | PPRArray (_, _, _, body, _, _) => newis_inside lab body
	  | PPoption (_, body) => newis_inside lab body
	  | _ => false

	fun try f list = case list of 
		h :: t => (f(h)  handle _ => try f t)
	 | nil => raise Empty 
	
	(* finds the Punion with a particular label in Ty *)
	fun sum_with_lab (lab:Id) (ty:Ty) = case ty of 
		Punion({coverage, label, ...}, tylist) => 
				(case label of SOME id =>
					(if Atom.same(id, lab) then ty
				 	else try (sum_with_lab lab) tylist)
				     | NONE => try (sum_with_lab lab) tylist)
		| Base _ => raise Empty
		| TBD _ => raise Empty
		| Pstruct (_, tylist) => try (sum_with_lab lab) tylist
		| Parray (_, {tokens, lengths, first, body, last}) => 
			try (sum_with_lab lab) [first, body, last]
		| RArray (_, _, _, body, _, _) => sum_with_lab lab body 
		| Poption (_, ty') => sum_with_lab lab ty'
		| _ => (print "the offending ty is \n"; printTy ty; raise TyMismatch) 

	fun newsum_with_lab (lab:Id) (ty:NewTy) = case ty of 
		PPunion({coverage, label, ...}, tylist) => 
				(case label of SOME id =>
					(if Atom.same(id, lab) then ty
				 	else try (newsum_with_lab lab) tylist)
				     | NONE => try (newsum_with_lab lab) tylist)
		| PPBase _ => raise Empty
		| PPTBD _ => raise Empty
		| PPstruct (_, tylist) => try (newsum_with_lab lab) tylist
		| PParray (_, {tokens, lengths, first, body, last}) => 
			try (newsum_with_lab lab) [first, body, last]
		| PPRArray (_, _, _, body, _, _) => newsum_with_lab lab body 
		| PPoption (_, ty') => newsum_with_lab lab ty'
		| _ => (print "the offending ty is \n"; printNewTy ty; raise TyMismatch) 

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

	fun newis_useless (ty: NewTy) (labList: Id list, determined:Id) =
	let
		(* a sum determines a value inside itself *)
		fun inside_sum() =
		let
			fun is_fun lab = let
				val sumty = newsum_with_lab lab ty
			in
				newis_inside determined sumty
			end handle PPEmpty => false
		in
			case labList of 
			  [h] => is_fun h
			|  _ => false
		end 
		
		(* a value inside a sum determines the sum *)
		fun determines_from_inside() = 
		let
			val sumty = newsum_with_lab determined ty
		in
			List.exists (fn x => newis_inside x sumty) labList 
		end handle Empty => false
	in
		List.exists (fn x => x()) [ inside_sum, determines_from_inside ]
	end

(* the main constraining function. Takes a constraint map and a Ty, and returns an updated
constraint map *)
	exception TableTooLarge
	fun constrainTy (ty, cmap) = 
 	let
		(* make the table *)
(*
		val _ = print ("Building table for ("^Int.toString (getCoverage ty)^")\n")
		val _ = printTy ty
*)
		val (_, tytable) = Table.genTable (getCoverage(ty)) ty
(*
		val _ = print ("Number of records: "^ Int.toString(getnumrecords(ty)) ^"\n")
*)
		val header = #1 tytable
		val bdocols = #2 tytable
		val bdolist = transpose(bdocols)

		(* The following is a simple implementation of 1-1 functional dependency analysis*)
		val (_, bdomap) = foldl (fn ((col:Token option list), (n, mymap)) => 
				(n+1, IntMap.insert(mymap, n, col))) (0, IntMap.empty) bdocols
 	        (* filter out the constant cols *)
 		fun nonconst (col:Token option list) = let
			val myset = BDOSet.addList (BDOSet.empty, col)
			in if (BDOSet.numItems myset) = 1 then false
			   else true
			end
		val bdomap = IntMap.filter nonconst bdomap
		val indexed_collist: (int*Token option list) list = IntMap.listItemsi bdomap
		fun pair (i, col) iclist = map (fn ic => ((i, col), ic)) iclist
		fun pairall iclist =
			case iclist of 
			  nil => nil
			  | (hd::rest) => (pair hd rest) @ (pairall rest)

		(*function to determine if two token option list are functionally dependent*)
		fun dependent (map, col1, col2) = 
		    case (col1, col2) of
			(nil, nil) => true
			| (to1::tail1, to2::tail2) =>
			  (	
			    case Map.find (map, to1) 
				of NONE => dependent (Map.insert(map, to1, to2), tail1, tail2)
				| SOME x => if compare (x, to2) = EQUAL then dependent (map, tail1, tail2)
					    else false
			  )
			
		val icol_pairs =  pairall indexed_collist
		val dependent_pairs = List.filter (fn ((i1, col1), (i2, col2)) => 
					dependent (Map.empty, col1, col2)) icol_pairs
		val deps = List.foldl (fn (((i1, col1), (i2, col2)), deplist) =>
			([i1], i2)::(([i2], i1)::deplist)) nil dependent_pairs
(*
		val _ = print ("The number of columns : " ^ Int.toString(length(header)) ^"\n")
		val _ = print ("The number of rows: " ^ Int.toString(length(bdolist)) ^"\n")
		val _ = if (length(bdolist) > DEF_MAX_TABLE_ROWS andalso 
				length(header)> DEF_MAX_TABLE_COLS) 
			then (print "Table too large: bailing out...\n"; raise TableTooLarge )
			else 0
*)
		val _ = if Options.print_tables then 
			Table.printTable(header, bdolist) else ()
(*
		(* take a table entry and run TANE on it, find the constraints that apply *)
		val partitions = initPartitionMap(bdolist, bdocols)
		val (deps,keys) = FunctionalDep.tane(partitions)
*)
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
		val labeled_keys = map (map (fn x => List.nth(header,x))) keys
	        val _ = if Options.print_functional_deps then print ("Dependencies ("^Int.toString(length(labeled_deps))^"):\n") 
							else ()
	        val _ = if Options.print_functional_deps 
	              then app printDep labeled_deps
	              else ()
		val _ = if Options.print_functional_deps 
		         then app (fn l => 
				print ("Key {" ^ implode(idstostrs(l),",")  ^ "}\n") 
				) labeled_keys
		         else ()
*)
		
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
			LabelMap.insert(cm,label,map #1 constraints)) cmap consts
	end handle TableTooLarge => cmap

	fun constrainNewTy (ty, cmap) = 
 	let
		val (_, tytable) = Table.genNewTable (getNCoverage(ty)) ty
(*
		val _ = print ("Number of records: "^ Int.toString(getnumrecords(ty)) ^"\n")
*)
		val header = #1 tytable
		val bdocols = #2 tytable
		val bdolist = transpose(bdocols)

		(* The following is a simple implementation of 1-1 functional dependency analysis*)
		val (_, bdomap) = foldl (fn ((col:BSToken option list), (n, mymap)) => 
				(n+1, IntMap.insert(mymap, n, col))) (0, IntMap.empty) bdocols
 	        (* filter out the constant cols *)
 		fun nonconst (col:BSToken option list) = let
			val myset = NewBDOSet.addList (NewBDOSet.empty, col)
			in if (NewBDOSet.numItems myset) = 1 then false
			   else true
			end
		val bdomap = IntMap.filter nonconst bdomap
		val indexed_collist: (int*BSToken option list) list = IntMap.listItemsi bdomap
		fun pair (i, col) iclist = map (fn ic => ((i, col), ic)) iclist
		fun pairall iclist =
			case iclist of 
			  nil => nil
			  | (hd::rest) => (pair hd rest) @ (pairall rest)

		(*function to determine if two token option list are functionally dependent*)
		fun dependent (map, col1, col2) = 
		    case (col1, col2) of
			(nil, nil) => true
			| (to1::tail1, to2::tail2) =>
			  (	
			    case NewMap.find (map, to1) 
				of NONE => dependent (NewMap.insert(map, to1, to2), tail1, tail2)
				| SOME x => if newcompare (x, to2) = EQUAL then dependent (map, tail1, tail2)
					    else false
			  )
			
		val icol_pairs =  pairall indexed_collist
		val dependent_pairs = List.filter (fn ((i1, col1), (i2, col2)) => 
					dependent (NewMap.empty, col1, col2)) icol_pairs
		val deps = List.foldl (fn (((i1, col1), (i2, col2)), deplist) =>
			([i1], i2)::(([i2], i1)::deplist)) nil dependent_pairs
		val _ = if Options.print_tables then 
			Table.newprintTable(header, bdolist) else ()
		fun depToLabel( alist : int list, a : int):Id list*Id = 
			(map (fn b => List.nth(header,b) ) alist, List.nth(header,a) )
		(* labeled_deps is (Id list * Id) list *)
		val labeled_deps = map depToLabel deps
			
		(* filter out the useless deps *)
		val (labeled_deps:(Id list * Id) list, deps) = 
			ListPair.unzip( List.filter (fn (x,_) => not (newis_useless  ty x)) 
					(ListPair.zip(labeled_deps,deps)))			
		(* determine what constraints apply *)
		val dpl = map (dep_cols bdocols) deps
		val labl:(Id list * Id) list = map (dep_cols header) deps
		val dpl = map (fn (y,x) => (y,map (fn z => [z]) x)) dpl
		val found_deps: (Id*newconstraint) option list = 
			map newdetermine_dep (ListPair.zip(labl,dpl) )
		val found_deps = List.filter (fn x => case x of 
			SOME _ => true | NONE => false) found_deps
		val found_deps:(Id* newconstraint) list = map some found_deps

 		(* find the single column constraints *)
		val a = (Int.toLarge(some(Int.maxInt)), Int.toLarge(some(Int.minInt)))
		(* initialize each entry to some starting values *)
		val consts = newzip1([ NRange a, NLength 0, NOrdered NAscend, 
				    NOrdered NDescend, NUnique((PPempty, "")), 
				    NEnumC NewBDSet.empty ], NONE)
		(*depstart is a list of constraints of size num of cols in the table*)
		val depstart:newconstraint_record = map (fn x=> {label = x, 
			constraints = consts:(newconstraint*BSToken option) list, 
			previous_values= NewBDSet.empty}) header
		val consts:newconstraint_record = foldr newupdate depstart bdolist (* returns a constraint_record *)

		(* combine the constraints found through dep analysis with the single column constraints *)
		fun add_to_consts consts (lab, dep) = 
		  map (fn {label,constraints, previous_values} => 
		    if Atom.same(label, lab) 
		      then {label=label, constraints=(dep,NONE)::constraints, previous_values=previous_values} 
		      else {label=label, constraints=constraints, previous_values=previous_values}
		     ) consts
		val consts = foldr (fn (dep, c) => add_to_consts c dep) consts found_deps
	in foldr (fn ({label,constraints,previous_values},cm) => 
			LabelMap.insert(cm,label,map #1 constraints)) cmap consts
	end handle TableTooLarge => cmap

	fun constrain' ty =
	let
		val arrays= Table.parseArrays ty
		val newarrays = case ty of 
				Parray _ => arrays
			|	RArray _ => arrays
			|	_ => ty::arrays	
	in
		foldl constrainTy LabelMap.empty newarrays
	end

	fun newconstrain' ty =
	let
		val arrays= Table.parsePPArrays ty
		val newarrays = case ty of 
				PParray _ => arrays
			|	PPRArray _ => arrays
			|	_ => ty::arrays	
	in
		foldl constrainNewTy LabelMap.empty newarrays
	end

end
