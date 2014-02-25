(* Zach DeVito
The main datatypes that the system uses
*)
structure DataTypes = struct
type regexp = unit   (* placeholder for now *)

(* Base types
*)
datatype base = 
  IntBase            (* sequence of digits *)
| LettersBase        (* sequence of letters *)
| ConstBase of string (* a constant sequence of string *)
| REBase of string   (* match regular expression *)

datatype BaseData = 
	Int of string * int
|	Letters of string
|	Const of string
|	RegEx of string

datatype IRData = 
  BaseD of BaseData
| TupleD of IRData list
| SumD of IRData * int
| ArrayD of IRData list	

(* defines an arbitrary order on base data
to put it in maps *)
fun compare(a,b) = case (a,b) of
		(SOME a', SOME b') => compared(a',b')
	|	(SOME a, _) => GREATER
	|	(NONE, SOME _) => LESS
	|	(NONE,NONE) => EQUAL
	and compared(a,b) = case a of
		Int (_,x) => (case b of   Int (_,x') => Int.compare(x,x')
							| Letters s => GREATER
							| Const c => GREATER
							| RegEx(c) => GREATER)
	|	Letters s => (case b of	Int x => LESS
							| Letters s' => String.compare(s,s')
							| Const c => GREATER
							| RegEx(c) => GREATER)

	|	Const c =>    (case b of	Int x => LESS
							| Letters s' => LESS
							| Const c' => String.compare(c,c')
							| RegEx(c) => GREATER)
	|	RegEx(c) => (case b of	Int x => LESS
							| Letters s' => LESS
							| Const c' => LESS
							| RegEx(c') => String.compare(c,c'))
fun base_equal(b1,b2) = 
	case(b1,b2) of 
	  (IntBase,IntBase) => true
	| (LettersBase,LettersBase) => true
	| (REBase a, REBase b) => a = b
	| (ConstBase a, ConstBase b) => a = b
	| _ => false
fun base_cmp(b1,b2) =
  	case(b1,b2) of 
	  (IntBase,IntBase) => EQUAL
	| (LettersBase,LettersBase) => EQUAL
	| (REBase a, REBase b) => String.compare(a,b)
	| (ConstBase a, ConstBase b) => String.compare(a,b)
	
	| (IntBase,LettersBase) => GREATER
	| (IntBase, REBase _) => GREATER
	| (IntBase, ConstBase _) => GREATER
	
	| (LettersBase,IntBase) => LESS
	| (LettersBase, REBase _) => GREATER
	| (LettersBase, ConstBase _) => GREATER
	
	| (REBase _, IntBase) => LESS
	| (REBase _, LettersBase) => LESS
	| (REBase _, ConstBase _) => GREATER
	
	| (ConstBase _,LettersBase) => LESS
	| (ConstBase _, REBase _) => LESS
	| (ConstBase _, IntBase) => LESS
	
	
structure BDSet = RedBlackSetFn(struct
							type ord_key = BaseData
							val compare = compared
							end)
type id = Label.id
(* The main IR type *)
datatype IR =
  Base of base 			(* a base value in the type *)
| Tuple of IR list      (* a sequence of IRs parsed in order *)
| Sum of IR list		(* a choice of one of the list of IRs, parsed in order *)
| Array of IR 			(* a repeated number of IR until it can no longer be parsed *)
| Label of id * IR		(* used to label a particular node *)
(* constraints associated with IRs *)
and constraint =
  Length of int                (* constrains array lengths, string lengths, # of int digits *)
| Ordered of ordered             (* constrains arrays to be in order *)
| Unique of BaseData              (* value is always the same when present, and is BaseData *)
| Range of int * int           (* constrains integers to fall in this range, inclusive *)
| Switched of id list * (BaseData option list * BaseData option) list (* a mapping between ids in id list, their values in the list of BaseData options, and the value of this node *)
| Eq of (id * Rat.rat) list * Rat.rat (* lin equation of id list plus constant *)
| Enum of BDSet.set (* set of values it takes on *)
and ordered = Ascend | Descend

fun re_equal(ir1, ir2) = 
	let
		fun myand(a,b) = a andalso b
		fun check_list(l1,l2) = 
		let 
			val bools = ListPair.map re_equal (l1, l2)
		in
			foldr myand true bools
		end
	in
		case (ir1,ir2) of
			  (Sum irlist,Sum irlist2) => check_list(irlist,irlist2)
			| (Tuple irlist,Tuple irlist2) => check_list(irlist,irlist2)
			| (Label(i1,r1),Label(i2,r2)) => i1 = i2 andalso re_equal(r1,r2)
			| (Base b1, Base b2) => base_equal(b1,b2)
			| (Array r1, Array r2) => re_equal(r1,r2)
			| _ => false
	end
	
fun relist_cmp(l2,l1) = 
  	case(l1,l2) of
		  (nil,nil) => EQUAL
		| (nil,h::t) => LESS
		| (h::t,nil) => GREATER
		| (h::t,h2::t2) =>
		  (case re_cmp(h,h2) of
		    EQUAL => relist_cmp(t,t2)
		  | LESS => LESS
		  | GREATER => GREATER )
and re_cmp(ir1, ir2) = 
		case (ir1,ir2) of
			  (Sum irlist,Sum irlist2) => relist_cmp(irlist,irlist2)
			| (Tuple irlist,Tuple irlist2) => relist_cmp(irlist,irlist2)
			| (Label(i1,r1),Label(i2,r2)) => re_cmp(r1,r2)
			| (Array r1, Array r2) => re_cmp(r1,r2)
			| (Base b1, Base b2) => base_cmp(b1,b2)
			| (Sum _, Tuple _) => GREATER
			| (Sum _, Label _) => GREATER
			| (Sum _, Array _) => GREATER
			| (Sum _, Base _)  => GREATER
			
			| (Tuple _, Sum _)   => LESS
			| (Tuple _, Label _) => GREATER
			| (Tuple _, Array _) => GREATER
			| (Tuple _, Base _)  => GREATER
			
			| (Label _, Sum _)   => LESS
			| (Label _, Tuple _) => LESS
			| (Label _, Array _) => GREATER
			| (Label _, Base _)  => GREATER
			
			| (Array _, Sum _)   => LESS
			| (Array _, Tuple _) => LESS
			| (Array _, Label _) => LESS
			| (Array _, Base _)  => GREATER
			
			| (Base _, Sum _)   => LESS
			| (Base _, Tuple _) => LESS
			| (Base _, Label _) => LESS
			| (Base _, Array _)  => LESS
end