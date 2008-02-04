open Hmm.D

(* suppose we have 3 states a, b, c and 3 observations 1, 2, 3 *)
let ids = [| Some "a"; Some "b"; Some "c"|] 

let mypi = [|0.33; 0.33; 0.34|] 

let mya = [| [|0.1; 0.1; 0.1|];
    	   [|0.1; 0.1; 0.1|];
	   [|0.1; 0.1; 0.2|] |] 

let myaend = [|0.1; 0.1; 0.1|] 

let myb = [| [|0.1; 0.1; 0.1|];
        [|0.1; 0.1; 0.1|];
	[|0.1; 0.1; 0.2|] |] 

(*
let test = make ~st:[| None ; None |] ~pi:[| 1. ; 0. |] 
  ~a:[| [| 0.1 ; 0.9 |] ; [| 1. ; 0. |] |]
  ~b:[| [| 0.6 ; 0.4 ; 0. |] ; [| 0.3 ; 0.4 ; 0.3 |] |]
  ~aend:[| 0.; 0. |]
*)

let h = make ~st:ids ~pi:mypi ~a:mya ~b:myb ~aend:myaend ()

let ta = [|2; 1; 1|];;
 
viterbi h ta
