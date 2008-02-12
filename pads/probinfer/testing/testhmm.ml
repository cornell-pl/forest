open Hmm.D

(* suppose we have 3 states a, b, c and 3 observations 1, 2, 3 *)
let rootpath= if (Array.length Sys.argv) > 1 then Sys.argv.(1) 
	      else ".."
let idfile = rootpath ^ "/training/TokenName"
let pifile = rootpath ^ "/training/InitProb"
let afile = rootpath ^ "/training/TransProb"
let aendfile = rootpath ^ "/training/EndProb"
let bfile = rootpath ^ "/training/EmitProb"
let inputfile = rootpath^"/testing/input"
let outputfile = rootpath^"/testing/output"

(*
let ids = [| Some "a"; Some "b"; Some "c"|] 

let mypi = [|0.33; 0.33; 0.34|] 

let mya = [| [|0.1; 0.1; 0.1|];
    	   [|0.1; 0.1; 0.1|];
	   [|0.1; 0.1; 0.2|] |] 

let myaend = [|0.1; 0.1; 0.1|] 

let myb = [| [|0.1; 0.1; 0.1|];
        [|0.1; 0.1; 0.1|];
	[|0.1; 0.1; 0.2|] |] 
*)

let transpose (m: float array array) =
  let m' = Array.make_matrix (Array.length m.(0)) (Array.length m) 0. in
  for i = 0 to ((Array.length m) - 1) do
    let row_i = m.(i) in
    for j = 0 to ((Array.length row_i) - 1) do
      m'.(j).(i) <- row_i.(j)
    done
   done;
   m'

let to_float s =
  if String.contains s 'E' then
    let len = String.length s in
    let index = String.index s 'E' in 
    let deci = float_of_string (String.sub s 0 index) in
    let exp = float_of_string (String.sub s (index+2) (len - (index +2))) in
    deci ** (~-.exp)
  else
    float_of_string s

let rec readlines buf lines =
  try 
	let line = input_line buf in
	readlines buf (lines@[line]) 
  with End_of_file -> lines

let parse lines =
  let rec _parse lines parsed =
    match lines with
	[] -> parsed
	| l :: lines -> 
	  _parse lines (parsed @ [(Str.split (Str.regexp "[ \t]+") l)]) 
  in _parse lines []

let to_array mylist = 
  let rec _to_array _2dlist listofarrays =
	(
	match _2dlist with
	l :: tail -> let a = Array.of_list (List.map to_float l) 
		     in _to_array tail (listofarrays @ [a])
	| [] -> listofarrays
	)
  in Array.of_list (_to_array mylist [])

let to_intarray mylist = 
  let rec _to_array _2dlist listofarrays =
	(
	match _2dlist with
	l :: tail -> let a = Array.of_list (List.map int_of_string l) 
		     in _to_array tail (listofarrays @ [a])
	| [] -> listofarrays
	)
  in Array.of_list (_to_array mylist [])

let to_1darray strlist = 
  let flist = List.map to_float strlist in
  Array.of_list flist

let idbuf = open_in idfile
let pibuf = open_in pifile
let abuf = open_in afile
let aendbuf = open_in aendfile
let bbuf = open_in bfile

let ids = Array.of_list (List.map (fun id -> Some id) (readlines idbuf []))
let _ = print_string ("ids : " ^ string_of_int (Array.length ids) ^ "\n")
let mypi = to_1darray (readlines pibuf [])
let _ = print_string ("pi : " ^ string_of_int (Array.length mypi) ^ "\n")
let mya = to_array (parse (readlines abuf []))
let a_row = mya.(0)
let _ = print_string ("a: " ^ string_of_int (Array.length mya) ^ ", " ^
		string_of_int(Array.length a_row) ^ "\n")
let myaend = to_1darray (readlines aendbuf [])
let _ = print_string ("aend : " ^ string_of_int (Array.length myaend) ^ "\n")
let myb = transpose (to_array (parse (readlines bbuf [])))
let _ = print_string ("b : " ^ string_of_int (Array.length myb) ^ ", " ^
			string_of_int (Array.length myb.(0)) ^ "\n")

(*
let test = make ~st:[| None ; None |] ~pi:[| 1. ; 0. |] 
  ~a:[| [| 0.1 ; 0.9 |] ; [| 1. ; 0. |] |]
  ~b:[| [| 0.6 ; 0.4 ; 0. |] ; [| 0.3 ; 0.4 ; 0.3 |] |]
  ~aend:[| 0.; 0. |]
*)
let print_ints buf a = 
  for i = 0 to (Array.length a) - 1 do
    print_int a.(i); print_string " ";
    output_string buf ((string_of_int a.(i))^ " ")
  done

let _ = print_string "Done making the matrices!\n"

let _ = close_in idbuf
let _ = close_in pibuf
let _ = close_in abuf
let _ = close_in aendbuf
let _ = close_in bbuf

let h = make ~st:ids ~pi:mypi ~a:mya ~b:myb ~aend:myaend ()
 
let inbuf = open_in inputfile
let records = to_intarray (parse (readlines inbuf [])) (*list of lists*)
let _ = close_in inbuf
let outbuf = open_out outputfile;;

for i = 0 to (Array.length records) - 1 do
  let (x, a) = viterbi h records.(i) in
  if x = 0. then
    (print_float (x); print_string ": nil\n";
    output_string outbuf "nil\n")
  else
    (
    print_float (x); print_string ": ";
    print_ints outbuf a;
    print_string "\n";
    output_string outbuf "\n"
    )
done;;

let _ = close_out outbuf
let _ = print_string ("Output written to " ^ outputfile ^ "\n");;
