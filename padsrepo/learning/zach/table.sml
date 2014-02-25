(* Zach DeVito
   Handles the creation of tables from IR stuctures so they can be used in 
   constraint analysis
*)
structure Table = struct
	open Common
	structure LMap = Label.Map
	exception IRMismatch
	
	(* puts labels on an unlabeled IR *)
	fun addLabels(ir:IR):IR = case ir of
		  Base b => Label( Label.freshID (), ir)
		| Tuple irlist => Label(Label.freshID(),Tuple (map addLabels irlist))
		| Sum irlist => Label(Label.freshID(), Sum(map addLabels irlist))
		| Array ir => Label(Label.freshID(),Array(addLabels ir))
		| Label (l,ir') => ir
	
	(* determines the length of an IR in a table *)
	fun irlength (ir:IR):int = case ir of
		Base x => (case x of ConstBase _ => 0 | _ => 1)
	|	Tuple irlist => foldr op+ 0 (map irlength irlist)
	|	Sum irlist => 1 + foldr op+ 0 (map irlength irlist)
	|	Array _ => 1
	|	Label(_,ir) => irlength ir
	fun addEntry(table,label,entry) = 
		case LMap.find(table, label) of
		  SOME (lbs,x) => LMap.insert(table,label,(lbs,entry :: x))
		| NONE   => LMap.insert(table,label,(nil,[entry]))
						(* labels or data in table, Rows of table itself *)
	type table_map_entry = (Label.id list * BaseData option list list)
	
	exception Unimplemented
	(* adds the IR's data to the tables in tableMap *)
	fun addToTables( labeledIR : IR, irdata : IRData, tableMap : table_map_entry LMap.map)  =
	let 
	    (* creates a table row based on the irdata, calls itself recursively until it reaches base data types or an array *)
	    (* also keeps track of the labels that were used to find unused labels *)
	    (*returns (tableRow,newTableMap,newLabels) *)
		fun explodeIR tableMap usedLabels (labeledIR, irdata) = 
			case labeledIR of
			  Label(lb, ir) => (
				let 
				  val (row,newMap,newUsedLabels)= 
					case (irdata,ir) of
					(* constant type are not put into the table because they don't change *)
					  (BaseD(bd), Base _) => (case bd of Const _ => nil | _ => [SOME bd],tableMap,usedLabels)
					 (* apply recursively across tuples *)
					| (TupleD irdlist, Tuple irlist) => foldr (fn ( (ir',ird'), (list, tableMap',labels)) => 
																let val (a,newMap,labels') = explodeIR tableMap' labels (ir',ird')
																in (a @ list, newMap,labels')
																end
															  ) (nil,tableMap,usedLabels) (ListPair.zip(irlist,irdlist))
					(* apply the the specific option the data uses, then use the length of the other options in the sum
					to generate the correct number of NONEs in the table *)
					| (SumD(irdata',i),Sum irlist) => 
										  let 
											fun nones x = List.tabulate (x, fn _ => NONE)
											val start_length = foldr op+ 0 ( map irlength (List.take(irlist,i)) )
											val end_length = foldr op+ 0 (map irlength (List.drop(irlist,i + 1)))
											val (list, newMap,labels') = explodeIR tableMap usedLabels (List.nth(irlist,i),irdata')
											val label = case List.nth(irlist,i) of
														  Label(id,_) => id
														| _ => raise IRMismatch
										  in
											(SOME(Const label)::(nones start_length) @ list @ (nones end_length),
											newMap,labels')
										  end
					(* enter the length of the array in the current table, but add the entries of the array type to their own
					 seperate table *)
					| (ArrayD irdlist, Array ir') => 
					let
						val (newTableMap,newLabels) = foldr ( fn (ird, (tm,labels)) => let val (entry, tm',labels') = explodeIR tm labels (ir', ird)
																  in (addEntry(tm',lb,entry),labels')
																  end
												) (tableMap,usedLabels) irdlist
						val len = length irdlist
					in
						([SOME(Int(Int.toString len,len))], newTableMap,newLabels)
					end
					| (_,_) => (print (Interp.irdatatos irdata ^ Interp.irtos ir ); raise IRMismatch)
				in
					(row,newMap, Label.Set.add(newUsedLabels,lb) )
				end
				)
			| 	_	=> (print ("Unlabeled: " ^ Interp.irdatatos irdata ^ " " ^ Interp.irtos labeledIR ); raise Unimplemented)
		val (entry, newTableMap, usedLabels) = explodeIR tableMap Label.Set.empty (labeledIR, irdata)
	in
		(* the "BASE" Table holds the highest level entry before the first array *)
		(* all other entries are labeled by their Array's id *)
		(addEntry(newTableMap,Label.makeID "BASE",entry),usedLabels)
	end
	(* generates the list of labels that designate each columns label 
	It works parallel to explodeIR but uses only the headers
	This will return empty tables ready to be filled with explode IR
	*)
	fun genTableHeaders( labeledIR : IR, headerMap : table_map_entry LMap.map)  =
	let 
		fun explodeIR headerMap labeledIR = 
			case labeledIR of
			  Label(lb, ir) => (
				case ir of
					  Base b => (case b of ConstBase _ => nil | _ => [lb],headerMap)
					| Tuple irlist => foldr (fn ( ir', (list, headerMap')) => 
																let val (a,newMap) = explodeIR headerMap' ir'
																in (a @ list, newMap)
																end
															  ) (nil,headerMap) irlist
					| Sum irlist => let 
									  val (labelList, hm) = (foldr (fn ( ir', (list, headerMap')) => 
																let val (a,newMap) = explodeIR headerMap' ir'
																in (a @ list, newMap)
																end
															  ) (nil,headerMap) irlist)	
									in
										(lb :: labelList, hm)
									end
					| (Array ir') => 
					let
						val (list, newHeaderMap) = explodeIR headerMap ir'
						val newHeaderMap' = LMap.insert(newHeaderMap,lb, (list,nil))
					in
						([lb], newHeaderMap')
					end
					| _ => (print (Interp.irtos ir ); raise IRMismatch)
				)
			| 	_	=> (print ( Interp.irtos labeledIR ); raise Unimplemented)
		val (entry, newTableMap) = explodeIR headerMap labeledIR
	in
		LMap.insert(newTableMap,Label.makeID "BASE",(entry,nil))
	end
	fun printHeaders(hlist) = (app (fn x => print (x ^ " " )) hlist; print "\n")
	fun printTableMap(tableMap) = LMap.appi ( fn(lb,(head,table)) => (print ("Table: "^ lb ^ "\n"); printHeaders(head) ;app (fn x => print ((bdoltos x) ^ "\n")) table))  tableMap	
	
	(* labels the IR, generates the empty table using genTableHeaders, and then
	  fills the tables with data. Returns the new labeled IR, the tables it
	  generated and the labels that the data actually used
	*)
	 
	fun genTables( ir : IR, irdata : IRData ) : IR * table_map_entry LMap.map * Label.Set.set =
	let
		val labIR = addLabels(ir)
		val emptyTables = genTableHeaders(labIR, LMap.empty)
		val (tables,usedLabels) = addToTables(labIR, irdata, emptyTables)
	in
		(labIR,tables,usedLabels)
	end
end
(*

val lab = Table.addLabels(Interp.test3_desc);
val emptyTables = Table.genTableHeaders(lab, Table.LMap.empty); 
val tab = Table.addToTables(lab, Interp.test3_data, emptyTables);
Table.printTableMap(tab);
*)