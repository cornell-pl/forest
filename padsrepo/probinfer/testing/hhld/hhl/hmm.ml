(* Copyright (C) 2001-2005 Henri Binsztok
   
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
*)

(** Hidden Markov Models
    @author Henri Binsztok, 2001-2005
*)

open Printf

exception Hmm of string

type bw_default_pars = { smooth : bool
		       ; do_a : bool
		       ; do_b : bool
		       ; do_pi : bool
		       ; itermax : int
		       ; eps : float }

(* to avoid unbound type parameter: [> `add_first | `trans ] *)
type dot_par = Add_first | Trans 
type dot_default_pars = dot_par list

module type T =
sig
  type index = int
  type symbol = int
  type path = index array
  type obs = symbol array

  type hmm    
  type emission
  (** parameters for Baum-Welch EM reestimation *)
  type bw_pars
  (** parameters for dot_ouput
      FIXME: make a separate Dot module with functions and such type defitions *)
  type dot_pars

  (* Definitions *)

  val empty : hmm
  val size : hmm -> int
  val make : st:string option array -> pi:float array -> 
    a:float array array -> b:emission array ->
    ?aend:float array -> unit -> hmm
  val transitions : hmm -> index -> float array
  val emission : hmm -> index -> emission
  val pi : hmm -> index -> float
  val end_transition : hmm -> index -> float
  val name : hmm -> index -> string option

  (* Algorithms *)

  val generate : hmm -> int -> obs
  (* Rabiner: Problem 1 *)
  val forward : hmm -> obs -> float array array * float
  val backward : hmm -> obs -> float array array * float
  (* Rabiner: Problem 2 *)
  val viterbi : hmm -> obs -> float * path
  (* Rabiner: Problem 3 *)
  val default_pars : bw_pars
  val baumwelch : bw_pars -> hmm -> obs array -> hmm

  (* Conversions *)

  val to_string : hmm -> string
  val to_dot : dot_pars -> hmm -> string
  val to_generic : Generic.label -> hmm -> Generic.t_data
  val of_generic : Generic.label -> Generic.t_data -> hmm
end

(** discrete HMMs *)
module D : (T with type bw_pars = bw_default_pars 
              with type dot_pars = dot_default_pars 
              with type emission = float array) =
struct
  type index = int
  type symbol = int
  type path = index array
  type obs = symbol array

  type emission = float array
  type hmm = {
    (** state names (optional, mostly for pretty-pretting) *)
    st : string option array ;
    (** initial probabilities *)
    pi : float array ;
    (** transitions *)
    a : float array array ; 
    (** transitions to final state *)
    aend : float array ;   
    (** emission p.d.f. *)
    b : emission array
  }
  type bw_pars = bw_default_pars
  type dot_pars = dot_default_pars
  (* Definitions *)

  let empty = { st = [||] ; pi = [||] ; a = [||] ; b = [||] ; aend = [||]}

  let size hmm = Array.length hmm.st
    
  let check st a b pi aend =
    let n = Array.length st in
    Matrix.valid a &&
      Matrix.valid b &&
      (n = Array.length a) &&
      (n = Array.length a.(0)) &&
      (n = Array.length b) &&
      (n = Array.length pi) &&
      (n = Array.length aend)

  let make ~st ~pi ~a ~b ?aend () = 
    let aend = match aend with None -> Array.make (Array.length st) 0. | Some a -> a in
    if check st a b pi aend then 
    { st = st
    ; a = Array.map Myarray.normalize a
    ; b = Array.map Myarray.normalize b
    ; pi = Myarray.normalize pi
    ; aend = aend }
    else raise (Hmm "invalid parameters")

  let transitions h i = h.a.(i)
  let emission h i = h.b.(i)
  let pi h i = h.pi.(i)
  let name h i = h.st.(i)
  let end_transition h i = h.aend.(i)

  (* Algorithms *)

  let generate hmm length = 
    Random.self_init () ;
    let rec gen_seq_rec i l nb_obs =
      if nb_obs >= length then l
      else 
	let new_obs = Maths.which hmm.b.(i) in
	try (
	  gen_seq_rec (Maths.which hmm.a.(i)) (new_obs :: l) (succ nb_obs)
	) with _ -> new_obs :: l
    in
    Array.of_list (
      List.rev (
	gen_seq_rec (Maths.which hmm.pi) [] 0
      )
    )

  let forward hmm o =
    let bigt = Array.length o
    and bign = Array.length hmm.st in
    let alpha = Array.make_matrix bigt bign 0. in
    for t = 0 to pred bigt do
      for j = 0 to pred bign do
	alpha.(t).(j) <- hmm.b.(j).(o.(t)) *. (
	  if t = 0 then hmm.pi.(j)
	  else Maths.sum ~n:bign ~f:(fun i -> alpha.(t - 1).(i) *. hmm.a.(i).(j))
	)
      done
    done ;
    (alpha, Myarray.sum alpha.(pred bigt))

  let backward hmm o =
    let bigt = Array.length o
    and bign = Array.length hmm.st in
    let beta = Array.make_matrix bigt bign 0. in
    for t = pred bigt downto 0 do
      for i = 0 to pred bign do
	beta.(t).(i) <-
	  if t = pred bigt then 1.
	  else
	    let f j = hmm.a.(i).(j) *. hmm.b.(j).(o.(t+1)) *. beta.(t+1).(j) in
	    Maths.sum ~n:bign ~f
      done
    done ;
    let f i = hmm.pi.(i) *. hmm.b.(i).(o.(0)) *. beta.(0).(i) in
    let likelihood = Maths.sum ~n:bign ~f in
    beta, likelihood

  let viterbi hmm o =
    let bigt = Array.length o
    and bign = Array.length hmm.st in
    let delta = Array.make_matrix bigt bign 0.
    and psi = Array.make_matrix bigt bign 0 in
    for t = 0 to pred bigt do
      for i = 0 to pred bign do
	if t = 0 then 
	  delta.(t).(i) <- hmm.pi.(i) *. hmm.b.(i).(o.(t))
	else
	  let f j = delta.(t - 1).(j) *. hmm.a.(j).(i) in
	  let max, argmax = Maths.max_and_arg ~n:bign ~f in
	  delta.(t).(i) <- max *. hmm.b.(i).(o.(t)) ;
	  psi.(t).(i) <- argmax
      done
    done ;
    let p_star = Myarray.max delta.(pred bigt)
    and q_star = Myarray.argmax delta.(pred bigt) in
    let path = ref [q_star] 
    and q = ref q_star in
    for t = bigt - 2 downto 0 do
      q := psi.(t + 1).(!q) ;
      path := !q :: !path
    done ;
    (p_star, Array.of_list !path)
      

  let update_bw ~hmm ~n ~bigt ~sum_xi ~alpha ~beta ~sum_gamma_k ~sum_gamma0 ~o ~likelihood =
    for t = 0 to bigt - 2 do
      for i = 0 to pred n do
	for j = 0 to pred n do
	  sum_xi.(i).(j) <- sum_xi.(i).(j) +.
	    alpha.(t).(i) *. hmm.a.(i).(j) *. hmm.b.(j).(o.(t + 1)) *. beta.(t + 1).(j) 
	  /. likelihood
	done
      done
    done ;
    for t = 0 to pred bigt do
      for i = 0 to pred n do
	sum_gamma_k.(i).(o.(t)) <- sum_gamma_k.(i).(o.(t)) +. 
	  alpha.(t).(i) *. beta.(t).(i) /. likelihood
      done
    done ;
    for i = 0 to pred n do
      sum_gamma0.(i) <- sum_gamma0.(i) +. 
	alpha.(0).(i) *. beta.(0).(i) /. likelihood
    done

  let reestime_baum_welch_one_iter 
      ?(do_a=true) ?(do_b=true) ?(do_pi=true) ?(smooth=false) hmm data =
    let n = Array.length hmm.st
    and m = Array.length hmm.b.(0) in
    let sum_xi = Array.make_matrix n n 0.
    and sum_gamma_k = Array.make_matrix n m 0. 
    and sum_gamma0 = Array.make n 0. in
    let sum_loglikelihood =
      Array.fold_left (
	fun acc o -> 
	  let bigt = Array.length o in
	  let (alpha, likelihood) = forward hmm o 
	  and beta = fst (backward hmm o) in
	  assert (likelihood > 0.) ;
	  update_bw ~bigt ~n ~alpha ~beta ~hmm ~o ~likelihood ~sum_xi ~sum_gamma_k ~sum_gamma0 ;
	  acc +. log likelihood
      ) 0. data in
    { st = hmm.st
    ; a = if do_a then Matrix.normalize ~smooth sum_xi else hmm.a
    ; b = if do_b then Matrix.normalize ~smooth sum_gamma_k else hmm.b
    ; pi = if do_pi then Myarray.normalize ~smooth sum_gamma0 else hmm.pi
    ; aend = hmm.aend (* FIXME: pas de réestimation de l'arrêt *)
    }, sum_loglikelihood
      
  let default_pars = { smooth = false
		     ; do_a = true
		     ; do_b = true
		     ; do_pi = true
		     ; itermax = 10
		     ; eps = 1e-3 }

  let baumwelch pars hmm data = 
    let rec learn_rec hmm i old_ll =
      let new_hmm, new_ll = 
	reestime_baum_welch_one_iter hmm data 
	  ~smooth:pars.smooth ~do_a:pars.do_a ~do_b:pars.do_b ~do_pi:pars.do_pi in
      eprintf "iter %d : ll %.5f %.5f\n" i new_ll (new_ll -. old_ll) ; flush stderr ;
      if i < pars.itermax && new_ll -. old_ll > pars.eps
      then learn_rec new_hmm (i + 1) new_ll
      else new_hmm in
    learn_rec hmm 0 neg_infinity

  (* Conversions *)

  (* FIXME: incomplete *)
  let to_string h =
    Myarray.show string_of_float h.pi ~name:"Pi" ^
      Matrix.to_string h.a ~name:"A (transitions)" ^
      Matrix.to_string h.b ~name:"B (emissions)"

  (* FIXME: bien mis à jour à partir de Visual/Probmod... 
     coloriser, factoriser les styles..., fonctionnel... 
     unifier avec to_dot des HHMM
  (* match m.a.(i).(j) with
     | p when p < 1E-5 -> ""
     | p when p > 0.5 -> atob ~opt:"style=bold" (node i) (node j) p
     | p when p < 0.05 -> atob ~opt:"style=dotted" (node i) (node j) p
     | p -> atob (node i) (node j) p *)
  *)
  let to_dot options m =
    let node i = "node" ^ string_of_int i
    and atob ?(opt="") a b f =
      sprintf "%s -> %s [%s%s] ;\n" a b
	(if List.mem Trans options then sprintf "label=\"%s\"," (Mystring.of_float f) else "") opt
    and graph t s = "digraph " ^ t ^ " {\n" ^ s ^ "}\n" 
    and tstyle = function
      | 1. -> ""
      | f -> sprintf "label=\"%s\",color=\"#886666\",fontcolor=\"#886666\"" 
	  (if List.mem Trans options then Mystring.of_float f else "")
    in
    let trans a b = function
      | f when f < 1E-5 -> ""	
      | f -> atob a b f ~opt:(tstyle f) in
    let s = ref (
      (if List.mem Add_first options then 
	 "start [style=filled,color=black,fontcolor=white] ;\n" else "") ^
	(if (Array.fold_left (fun acc x -> acc or x>0.) false m.aend) then 
	   "end [style=filled,color=black,fontcolor=white] ;\n"
	 else "")
    ) in
    for i = 0 to Array.length m.st - 1 do
      s := !s ^ (
	node i ^ (	 
	  match m.st.(i) with
	  | None -> sprintf "[label=%s,style=filled,color=\"#AACCEE\"]" (node i)
	  | Some lb -> sprintf "[label=\"%s\",style=filled,color=\"#EECCAA\"]" lb
	) ^ " ;\n"
      ) ;
      if List.mem Add_first options && m.pi.(i) > 0. then
      s := !s ^ trans "start" (node i) m.pi.(i)
    done ;
    for i = 0 to pred (Array.length m.st) do
      for j = 0 to pred (Array.length m.st) do
	s := !s ^ trans (node i) (node j) m.a.(i).(j)
      done ;
      s := !s ^ trans (node i) "end" m.aend.(i)
    done ;
    graph "Hmm" !s

  open Generic

  (* FIXME: make a variant or not? *)
  let to_generic hl h = 
    Variant (hl, Record [| array (option string) h.st 
			; array float h.pi  
			; array (array float) h.a 
			; array (array float) h.b 
			; array float h.aend |] )
      
  let of_generic hl r = 
    try 
      match r with
	(* old version without aend *)
      | Variant (l, Record [| st ; pi ; a ; b |]) when l = hl ->
	  { st = array_of (option_of string_of) st ;
	    pi = array_of float_of pi ;
	    a = array_of (array_of float_of) a ;
	    b = array_of (array_of float_of) b ;
	    aend = Array.make (length st) 0. }
      | Variant (l, Record [| st ; pi ; a ; b; aend |]) when l = hl ->
	  { st = array_of (option_of string_of) st ;
	    pi = array_of float_of pi ;
	    a = array_of (array_of float_of) a ;
	    b = array_of (array_of float_of) b ;
	    aend = array_of float_of aend }
      | _ -> raise (Hmm "not a HMM")
    with _ -> raise (Hmm "probably not a HMM")

end
  
open D

let test = make ~st:[| None ; None |] ~pi:[| 1. ; 0. |] 
  ~a:[| [| 0.1 ; 0.9 |] ; [| 1. ; 0. |] |]
  ~b:[| [| 0.6 ; 0.4 ; 0. |] ; [| 0.3 ; 0.4 ; 0.3 |] |]
  ~aend:[| 0.; 0. |]

