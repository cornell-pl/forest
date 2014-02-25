(* 
Zach DeVito
Reduce implements the simplification system for IRs
*)
structure Reduce = struct
open Common
(* calculates the complexity of a datatype so that we can try to minimize it*)
fun cost const_map ir =
	case ir of 
	  Base b => (case b of ConstBase _ => 1 | _ => 2) (* consts are cheaper than variables *)
	| Tuple irlist => foldr op+ 0 (map (cost const_map) irlist) + 3 (* bigger datastructures are more complex than bases*)
	| Sum irlist => foldr op+ 0 (map (cost const_map) irlist) + 3
	| Array ir' => 3 + (cost const_map ir')
	| Label (name,ir') => 
	  let
	  	val entry = Label.Map.find(const_map,name)
	  	val is_base = case ir' of Base _ => true | _ => false
	  	val e_cost = 
	  	  case entry of 
	  	    SOME x => foldr op+ 0 (map (const_cost is_base) x)
	  	  | NONE => 0
	  	val ir_cost = (cost const_map ir')
	  in
	  	e_cost + ir_cost + 1 (* every constraint is counted towards cost *)
	  end
and const_cost is_base c = 
  case c of
    Length _ => if is_base then 1 else 0
  | Ordered _ => if is_base then 1 else 0
  | Unique _ => if is_base then 1 else 0
  | Range _ => if is_base then 1 else 0
  | Switched (_,ops) => 5
  | Eq _ => 1
  | Enum set => if is_base then 5 else 0
  
type constraint_map = constraint list Label.Map.map

(* a rule takes an IR and the constrant lookup table, returns a [possibly] new IR *)

(* 	they are seperated into two types: pre and post constraint anaysis
	this allows for a reduction in complexity without worrying about labels
	changes due to major structural changes
*)
type pre_reduction_rule = IR -> IR
type post_reduction_rule = constraint_map -> Label.Set.set -> IR -> IR
(* reduction rules *)
(* single member lists are removed*)
fun remove_degenerate_list ir =
case ir of
  Sum (h :: nil) => h
| Tuple (h :: nil) => h
| Tuple nil => Base (ConstBase "")
| Sum nil => Base (ConstBase "")
| _ => ir
(* tuples inside tuples are removed*)
and unnest_tuples ir : IR =
case ir of 
  Tuple irlist =>
  let
  	fun lift_tuple ir = 
  	case ir of
  	  Tuple irlist => irlist
  	| _ => [ir]
  	val result = map lift_tuple irlist
  in
  	Tuple( List.concat result )
  end
| _ => ir
(* sums inside sums are removed *)
and unnest_sums ir : IR = 
case ir of 
  Sum irlist =>
  let
  	fun lift_sum ir = 
  	case ir of
  	  Sum irlist => irlist
  	| _ => [ir]
  	val result = map lift_sum irlist
  in
  	Sum( List.concat result )
  end
| _ => ir
(* remove nil items from tuples *)
and remove_nils ir : IR = 
case ir of
  Tuple irlist => Tuple (List.filter 
    (fn x => case x of
      Base (ConstBase y) => not(y = "")
    | _ => true ) irlist)
| _ => ir
(* elements of a sum are check to see if they share a common prefix (ie tuples with
a common prefix, or a common postfix) these elements are then brought out of a sum
and a tuple is created *)
and prefix_postfix_sums ir : IR =
case ir of
  Sum irlist =>
  let
  	fun conv_to_list ir' =
  	case ir' of
  	  Tuple irlist' => irlist'
  	| _ => [ir']
  	val underlings = map conv_to_list irlist
  	fun commonPrefix irlists = 
  	let
  		val elems = map hd irlists
  		val tails = map tl irlists
  	in
		case elems of
		  h :: t => 
		  let 
		  	val not_equal = List.exists (fn x => not( re_equal(x,h) )) t
		  in 
		  	if not_equal then nil else h :: commonPrefix tails
		  end
		| nil => nil
  	end handle Empty => nil
  	val cpfx = commonPrefix underlings
  	val plen = length cpfx
  	val remaining = map (fn x => List.drop(x,plen) ) underlings
  	val remaining_rev = map List.rev remaining
  	val csfx_rev = commonPrefix remaining_rev
  	val slen = length csfx_rev
  	val remaining_rev = map (fn x => List.drop(x,slen) ) remaining_rev
  	val remaining = map List.rev remaining_rev
  	val csfx = List.rev csfx_rev
  	val rem_tups = map (fn x => Tuple(x) ) remaining
  	val rem_reduced = map (remove_degenerate_list) rem_tups
  in
  	case (cpfx, csfx) of
  	  (h::t,_) => Tuple (cpfx @ [ Sum rem_reduced ] @ csfx)
  	| (_,h::t) => Tuple (cpfx @ [ Sum rem_reduced ] @ csfx)
  	| (nil,nil) => Sum rem_reduced
  end
| _ => ir
(* adjacent constraints are fused together *)
and adjacent_consts ir : IR = 
case ir of 
  Tuple irlist => 
  let
  	 fun for_const while_const t x = 
  	 let
       val (clist,rest) = while_const(t)
     in
	  (x :: clist, rest)
     end
     
  	fun while_const irlist = case irlist of 
  		h::t => (case h of
  				  Base(ConstBase x) => for_const while_const t x
  				| Label(_,Base(ConstBase x)) => for_const while_const t x
  				| _ => (nil,irlist) )
  	  | nil => (nil,nil)
  	fun find_adj irlist = case irlist of
  	  h::t => (case h of
  	  			Base(ConstBase x) => 
  	  			let
  	  				val (clist,rest) = while_const(t)
  	  			in
  	  				Base(ConstBase(String.concat(x :: clist))) :: find_adj rest
  	  			end
  	  		  | Label(_,Base(ConstBase x)) =>
  	  		    let
  	  				val (clist,rest) = while_const(t)
  	  			in
  	  				Base(ConstBase(String.concat(x :: clist))) :: find_adj rest
  	  			end
  	  		  
  	  		  | _ => h :: find_adj t
  	           )
  	| nil => nil
  	val newirlist = find_adj irlist
  in
  	Tuple(newirlist)
  end
| _ => ir
		
(* post constraint rules, these require the cmap to be filled  and the 
data labeled *)

(* a unique Base type becomes a constant type *)
and uniqueness_to_const cmos labels ir =
case ir of
  Label(id,Base b) => 
  if Label.Set.member(labels,id) then
    (case Label.Map.find(cmos,id) of  
      SOME consts => 
      let
      	fun find_unique clist =
      	  case clist of
      	    Unique x :: t => SOME(x)
      	  | h :: t => find_unique t
      	  | nil => NONE
      in
       case find_unique consts of
         SOME x => Base (ConstBase (bdtos' x))
       | NONE => ir
      end
    | NONE => ir)
  else ir
| _ => ir
(* removed unused branches of sums *)
and unused_branches cmos labels ir =
case ir of 
  Sum irlist => 
  let
  	fun isUnused(ir) = ( 
  	  case ir of
  	    Label(id,_) => not (Label.Set.member (labels,id))
  	  | _ => false)
  	fun remove_unused() = 
  	let
  		val (unused,used) = List.partition isUnused irlist
  		val strs = map irtos unused
  		(*val _ = app (fn x=> print ("unused:" ^ x)) strs*)
  	in
  		used
  	end
  in
  	Sum (remove_unused())
  end
| _ => ir
(* mapping between sums is used to created a larger sum using the mappings to
created the limited set of options *)
and sum_mapping cmos labels ir =
case ir of 
  Tuple irlist =>
  let
  	fun is_labeled list = 
  	  case list of
  	    Label(_,_) :: t => is_labeled t
  	  | h :: t => false
  	  | nil => true
  	fun find_sum_labels list =
  	  case list of
  	    h :: t => 
  	      (case h of
  	        Label(id,Sum irlist) => 
  	          if is_labeled irlist 
  	            then id :: find_sum_labels t
  	            else find_sum_labels t
  	      | _ => find_sum_labels t
  	      )
  	  | nil => nil
  	(* first find the sums in this tuple *)
  	val slabels = find_sum_labels irlist
  	val rhs_map = ref(Label.Map.empty)
  	val lhs_map = ref(Label.Map.empty)
  	 
  	 fun count mapref item value = 
  	   case Label.Map.find(!mapref,item) of
  	     NONE => (mapref := Label.Map.insert(!mapref,item,value::nil) )
  	   | SOME x => (mapref := Label.Map.insert(!mapref,item,value :: x) )
  	 fun map_contains(m,item) = 
  	   case Label.Map.find(m,item) of
  	     SOME _ => true
  	   | NONE => false
  	 fun is_useful lab = 
  	   case Label.Map.find(cmos,lab) of
  	     SOME consts =>
  	       let
  	        fun is_useful_help const = 
  	          case const of 
  	            Switched (h::nil,oplist) =>(count rhs_map lab h; if List.exists (fn x => x = h) slabels then count lhs_map h (lab,oplist) else ())
  	          | _ => ()
  	       in
  	        List.app is_useful_help consts
  	       end
  	   | NONE => ()
  	 (* is_useful finds the Switched constraints for these sums and counts how many times
  	 each sum appears on the right hand side and left hand side of a dependency *)
  	 val _ = List.app is_useful slabels
  	 (* labels that are not in any Switched constraint are removed *)
  	 val useful_labs = List.filter (fn x => map_contains(!rhs_map,x) orelse map_contains(!lhs_map,x) ) slabels
  	 val lhs = Label.Map.listItemsi (!lhs_map)
  	 exception REFACTOR_ERROR
  	 
  	 (* takes a list of IRs and a list of replacement values for particular labels
  	 returns the list with the IRs replaced*)
  	 fun replace replacements list = 
  	   case list of
  	     Label(id,ir) :: t => 
  	       (case List.find (fn (x,ir') => id = x) replacements of
  	          SOME (_,ir') => ir'
  	        | NONE => Label(id,ir)
  	       ) :: replace replacements t
  	   | ir :: t => ir :: replace replacements t
  	   | nil => nil
  	 exception IR_NOT_FOUND
  	 (*takes a label and returns the ir it labels *)
  	 fun lab_to_ir ir lab =
  	   case ir of 
  	     Tuple (h :: t) => (lab_to_ir h lab handle IR_NOT_FOUND => lab_to_ir (Tuple t) lab)
  	   | Sum (h :: t) => (lab_to_ir h lab handle IR_NOT_FOUND => lab_to_ir (Sum t) lab)
  	   | Label(id,ir') => if lab = id then ir else lab_to_ir ir' lab
  	   | Array(ir') => lab_to_ir ir' lab
  	   | Base b => raise IR_NOT_FOUND
  	   | Sum nil => raise IR_NOT_FOUND
  	   | Tuple nil => raise IR_NOT_FOUND
  	 val big_ir = ir
  	 (* takes a dominate sum and the others and uses the switches to refactor it
  	 into a sum of tuples *)
  	 fun refactor dom sums list = 
  	   case list of 
  	    Label(id,Sum domlist) :: t => 
  	      if(id = dom) then
  	      let
  	      	fun create_replacements ditem = 
  	      	  case ditem of
  	      	    Label(dom_id,ir) => 
  	      	    let
  	      	      (* find which label the dominator label chooses for this
  	      	      particular sum *)
  	      	      fun get_replacement (id, branches) = 
  	      	        case branches of
  	      	          ([SOME(Const dom_val)],SOME(Const my_val)) :: t =>
  	      	            if dom_val = dom_id 
  	      	            then
  	      	              (id, my_val)
  	      	            else get_replacement(id,t)
  	      	        | h :: t => get_replacement(id,t)
  	      	        | nil => raise REFACTOR_ERROR
  	      	    	val rep_labels = map get_replacement sums
  	      	    	val replacements = map (fn (id,my_val) => (id,lab_to_ir big_ir my_val)) rep_labels
  	      	    in
  	      	      Tuple (replace ((dom,ditem) :: replacements) irlist)
  	      	    end
  	      	  | _ =>  raise REFACTOR_ERROR
  	      	val options = map create_replacements domlist
  	      in
  	      	Sum options
  	      end
  	      else refactor dom sums t
  	  | h :: t => refactor dom sums t
  	  | nil => raise REFACTOR_ERROR
  in
    case lhs of 
      h :: t => 
      let
        (* find the label that appears most on lhs of constraints, use it
        to refactor the other values that appear on the rhs of its constraints*)
        fun max ((a,l1),(b,l2)) = if length l1 > length l2 then (a,l1) else (b,l2)
        val (dom,sums) = foldr max h t
        val newIR = refactor dom sums irlist
          handle REFACTOR_ERROR => (print "An error has occured trying to refactor a sum; giving up and returning the old IR\n"; ir)
               | IR_NOT_FOUND => (print "An error has occured trying to refactor a sum; giving up and returning the old IR\n"; ir) 
        val reducedIR = reduce NONE newIR
       (* val _ = print ("Candidate Sum:\n" ^ (irtos reducedIR)) *)
      in
      	reducedIR
      end
    | nil => ir
  end 
| _ => ir
(* remove a label nested in a label *)
and double_label_elim ir =
case ir of
  Label(id,Label(id2,ir')) => Label(id2,ir')
| _ => ir
and enum_to_sum cmap usedLabels ir = 
case ir of 
  Label(id,Base b) =>
    (case Label.Map.find(cmap,id) of 
      SOME consts => 
        let
        	fun check_enum list = 
        	  case list of 
        	    (Enum set) :: t =>
        	      let
        	      	val items = BDSet.listItems set
        	      	val strs = map bdtos' items
        	      	val c_types = map ConstBase strs
        	      	val bc_types = map Base c_types
        	      	val ir' = Sum (rev bc_types)
        	     (* 	val _ = print ("ENUM" ^ (irtos ir')) *)
        	      in
        	      	Label(id,ir')
        	      end
        	  | h :: t => check_enum t
        	  | nil => ir
        in
        	check_enum consts
        end
    | NONE => ir)
| _ => ir
(* the actual reduce function can either take a SOME(const_map,usedLabels) or
NONE.  It will use the constraints that it can apply. *)
and reduce const_info_op ir = 
let
  val pre_constraint_rules : pre_reduction_rule list = 
		[ 	remove_degenerate_list,
			unnest_tuples,
			unnest_sums,
			prefix_postfix_sums,
			adjacent_consts,
			remove_nils,
			double_label_elim
		]
  val post_constraint_rules : post_reduction_rule list =
		[ uniqueness_to_const,
		  unused_branches,
		  enum_to_sum,
		  sum_mapping
		]
  (* generate the lust of rules *)
  fun wrapper f cmap usedLabels ir = (f ir) 
  val common_rules = map (fn x => wrapper x Label.Map.empty Label.Set.empty) pre_constraint_rules
  val (constraint_rules,cmap) = 
    case const_info_op of 
      SOME (cmap,usedLabels) => (map (fn x => x cmap usedLabels) post_constraint_rules @ common_rules,cmap)
    | NONE => (common_rules, Label.Map.empty)
  fun reduce' ir =
    let 
      (* go bottom up, calling reduce' on children values first *)
      val reduced_children = 
			case ir of
			  Tuple irlist => Tuple((map reduce' irlist))
			| Sum irlist => Sum((map reduce' irlist))
			| Array ir' => Array((reduce' ir') )
			| Base b => Base b
			| Label(lb,ir') => Label(lb,(reduce' ir'))
	  fun iterate ir = 
	  let
	  	(* calculate the current cost *)
	    val cur_cost = cost cmap ir
	    (* apply each rule to the ir *)
	    val newIRs = map (fn x => x ir) constraint_rules
	    (* find the costs for each one *)
	    val costs = map (cost cmap) newIRs
	    val pairs = ListPair.zip(newIRs,costs)
	    fun min((a,b),(c,d)) = if b < d then (a,b) else (c,d)
	    (* find the minimum cost out of the ones found *)
	    val (newIR,lowCost) = foldr min (ir,cur_cost) pairs
	  in
	  	(* as long as the cost keeps going down, keep iterating *)
	  	if lowCost < cur_cost then iterate newIR
	  	else newIR
	  end
	in
	 (iterate reduced_children)
	end
  val cbefore = cost cmap ir
  val ir' = reduce' ir
  val cafter = cost cmap ir'
(*  val _ = print ("Before:" ^ (Int.toString cbefore) ^ " After:" ^ (Int.toString cafter) ^ "\n") *)
in
  ir'
end
(* now some tests *)
fun dotest() = 
let
	val a = Base IntBase
	val b = Base LettersBase
	val c = Base (ConstBase "c")
	val d = Base (REBase "*")
	val pre = Tuple [ a, b, d ]
	val post = Tuple [d, b, a ]
	
    val cmap_info = [ ("lab1", [Unique (Letters "a_unique_value")]), ("lab2",[]),("lab3",[Unique (Letters "a_unique_value")]), ("lab4",[Range (0,1)]) ]
	val labels = ["lab1","lab3","lab4"]
	
	fun wrap lab = SOME(Const lab)
	val cmap_info = [ 
					  ("s1", [ Range (1,3),
					           Switched (["s3"],
					            [
					              ([wrap "r101"],wrap "r1"),
					              ([wrap "r102"],wrap "r1"),
					              ([wrap "r103"],wrap "r1"),
					              ([wrap "r104"],wrap "r2"),
					              ([wrap "r105"],wrap "r2"),
					              ([wrap "r106"],wrap "r2")
					            ]),
					            Switched (["s2"],
					            [
					              ([wrap "r10"],wrap "r1"),
					              ([wrap "r11"],wrap "r1"),
					              ([wrap "r12"],wrap "r2"),
					              ([wrap "r13"],wrap "r2")
					            ])
					          ] ),
					   ("s2", [ Range (5,6),
					           Switched (["s3"],
					            [
					              ([wrap "r101"],wrap "r10"),
					              ([wrap "r102"],wrap "r11"),
					              ([wrap "r103"],wrap "r11"),
					              ([wrap "r104"],wrap "r12"),
					              ([wrap "r105"],wrap "r13"),
					              ([wrap "r106"],wrap "r13")
					            ])
					          ] ) 
					   
					] @ cmap_info
					            
	val labels = ["r1","r2","r10","r11","r12","r13","r101","r102","r103","r104","r105","r106"] @ labels
	
	val cmap = foldr (fn (a,m) => Label.Map.insert'(a,m) ) Label.Map.empty cmap_info
	val labset = Label.Set.addList(Label.Set.empty,labels)
	
	val tests = [ Tuple [], Sum [], Tuple [ Base LettersBase ], Sum [ Base LettersBase ],
				  Tuple[ Sum [ Sum [] ] ],
				  Tuple[ b,Tuple[a,a,a,a],b,Tuple[b,b,a]],
				  Sum[ Sum[a,b],Sum[c,d],a],
				  Sum [ Tuple[ pre,a,post], Tuple[pre,b,post], Tuple[pre,c,post] ],
				  Sum [ Tuple[ pre,a,post], Tuple[pre,b], Tuple[pre,c,post] ],
				  Sum [ Tuple[ pre,a,post], Tuple[pre,b,post], Tuple[c,post] ],
				  Tuple[ c,c,c ],
				  Tuple[ b,c,c,b,c,c,b,c,c,c],
				  Label("lab1",b),
				  Label("lab2",b),
				  Sum[a,Label("lab2", b),c,d]
				 ]
				 
	val r1 = Label("r1", Base (ConstBase "1"))
	val r2 = Label("r2", Base (ConstBase "2"))
	val r10 = Label("r10", Base (ConstBase "10"))
	val r11 = Label("r11", Base (ConstBase "11"))
	val r12 = Label("r12", Base (ConstBase "12"))
	val r13 = Label("r13", Base (ConstBase "13"))
	val r101 = Label("r101", Base (ConstBase "101"))
	val r102 = Label("r102", Base (ConstBase "102"))
	val r103 = Label("r103", Base (ConstBase "103"))
	val r104 = Label("r104", Base (ConstBase "104"))
	val r105 = Label("r105", Base (ConstBase "105"))
	val r106 = Label("r106", Base (ConstBase "106"))
	
	val test = Tuple [ a,Label("s1",Sum [ r1, r2]),Label("s2",Sum[ r10,r11,r12,r13 ]), Label("s3",Sum[ r101,r102,r103,r104,r105,r106 ]),a]
	
(*	val _ = print (irtos test)
	val (newIr,cmap) = sum_mapping cmap labels test
	val _ = print (irtos newIr) *)
	val tests = tests @ [test]
	val pre_results = ( map (reduce (NONE)) tests )
	val results = ( map (reduce (SOME(cmap,labset))) pre_results )
	val strings = ListPair.zip(map irtos tests, map irtos results)
	val strings' = map (fn (x,y) => "From:\n" ^ x ^ "To:\n" ^ y ^ "\n\n\n") strings
	val _ = map print strings'
in
 strings
end
end