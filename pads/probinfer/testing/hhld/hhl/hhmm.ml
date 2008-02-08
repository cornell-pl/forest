(* Copyright (C) 2003-2005 Henri Binsztok
   
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

(** Hierarchical Hidden Markov Models
    @author Henri Binsztok, 2003-2005
    @todo 
    - Coq proof of the core
    - add del_state to API (only for non-connected states, otherwise undefined behavior)
    - replace asserts with exceptions
    - fully document
*)

open Printf

exception Hhmm of string

(** core definition. states are indexed by ints
    @todo
    - abstract type for index?
    - separate module type for transitions?
*)
module type T =
sig
  (** abstract type for transitions *)
  type t
  type index = int
  val empty_transitions : t
  val iter : (index -> float -> unit) -> t -> unit
  val fold : (index -> float -> 'a -> 'a) -> t -> 'a -> 'a
  (** abstract type for hhmm *)
  type h
  (** index of root state *)
  val root_state : index    
  (** make a HHMM of given card containing one internal state and its end state *)
  val root : int -> h
  val card : h -> int
  (** add an internal state and the associated end state to ensure unicity *)
  val add_internal : (*?vt:t -> ?ht:t ->*) h -> p:index -> pi:float -> h * index
  val add_normal : h -> p:index -> pi:float -> a:t -> b:float array -> h * index 
  val end_of : h -> index -> index
  (** add submodel g to h, at parent p, with vertical probability pi *)
  val add_submodel : h -> p:index -> pi:float -> h -> h
  val size : h -> int
  val level : h -> index -> int
  val parent : h -> index -> index
  val children : h -> index -> index list
  val add_htransition : h -> index -> index -> float -> h
  val update_vtransition : h -> index -> index -> float -> h
  val state : h -> index -> [> `e_state | `i_state | `n_state ]
  val internal_h : h -> index -> t
  val internal_v : h -> index -> t
  (** probability of transition between state i and state j *)
  val transition : h -> index -> index -> float
  val normal_a : h -> index -> t
  val normal_b : h -> index -> float array
  val convert_intern_state : h -> index -> h
  val has_intern_states : h -> index -> bool
  (** conversion to generic for persistance *)
  val to_generic : h -> Generic.t_data
  val of_generic : Generic.t_data -> h
end

(** core implementation *)
module I : T =
struct
  type index = int
  module NodeType : (Map.OrderedType with type t = index) =
  struct
    type t = index
    let compare a b = - (compare a b)
  end
  module Node = Map.Make (NodeType)

  type transitions = float Node.t

  type internal_state = 
      { ht : transitions 
      ; vt : transitions
      ; children : index list (* not exactly redundant with vt *)
      ; iparent : index
      ; ilevel : int 
      ; iend : int }

  type end_state = 
      { eparent : index
      ; elevel : int }

  type normal_state = 
      { aqd : transitions
      ; bqd : float array
      ; nparent : index 
      ; nlevel : int }

  type state = 
    | Internal of internal_state
    | Normal of normal_state
    | End of end_state

  type hhmm = 
      { states : state Node.t
      ; card : int
      ; size : int }

  type h = hhmm
  type t = transitions

  let root_state = 0

  let iter = Node.iter
  let fold = Node.fold

  (** add/replace a node *)
  let node_remove k t = if Node.mem k t then Node.remove k t else t
  let node_add t k nv = Node.add k nv (node_remove k t)

  let parent h i = match Node.find i h.states with
  | Internal {iparent=p} | Normal {nparent=p} | End {eparent=p} -> p

  let children h i = match Node.find i h.states with
  | Internal {children=c} -> c | _ -> []

  let level h i = match Node.find i h.states with
  | Internal {ilevel=l} | Normal {nlevel=l} | End {elevel=l} -> l

  let state h i = match Node.find i h.states with
  | Internal _ -> `i_state
  | Normal _ -> `n_state
  | End _ -> `e_state

  (** internal function to add a state
      @todo
      - should return the state index (for proof)?
  *)
  let add h state =
    (* eprintf "node:%d\n" h.size ; flush stderr ; *)
    { size = succ h.size
    ; states = Node.add h.size state h.states 
    ; card = h.card }    

  let update h i ns = {h with states = node_add h.states i ns}
         
  let update_vtransition h i j f =
    let newstate = match Node.find i h.states with
    | Internal state -> 
	if parent h j = i then
	  let nvt = if f=0. then node_remove j state.vt else node_add state.vt j f in
	  (* and nchildren = if f=0. then Mylist.remove j state.children else j::state.children in *)
	  Internal {state with vt=nvt(*; children=nchildren*)}
	else raise (Hhmm (sprintf "destination state (%d) is not a child of parent (%d)" j i))
    | _ -> raise (Hhmm "can't add vertical transition to normal or end state") in
    update h i newstate

  let add_htransition h i j f =
    if parent h i = parent h j then
      let newstate = match Node.find i h.states with
      | Internal state -> 
	  let nht = if f=0. then node_remove j state.ht else node_add state.ht j f in
	  Internal {state with ht=nht}
      | Normal state -> 
	  let naqd = if f=0. then node_remove j state.aqd else node_add state.aqd j f in
	  Normal {state with aqd=naqd}
      | End _ -> raise (Hhmm "can't add a transition to end state") in
      update h i newstate
    else raise (Hhmm "horizontal transitions happen only between states with same parent")
      
  let empty_transitions:transitions = Node.empty

  (** internal function, dangerous as:
      - it assumes the transition does NOT yet exist (add child to parent)
      - it does NOT check whether i is parent of j
  *)
  let add_vtransition h i j f =
    let newstate = match Node.find i h.states with
    | Internal state -> 
	let nvt = (*if f=0. then node_remove j state.vt else*) node_add state.vt j f in
	(* and nchildren = if f=0. then Mylist.remove j state.children else j::state.children in *)
	Internal {state with vt=nvt ; children=j::state.children}
    | _ -> raise (Hhmm "can't add vtransition to normal or end state") in
    update h i newstate

  let add_internal (*?(vt=empty_transitions) ?(ht=empty_transitions)*) h  ~p  ~pi =    
    (* is : h.size
       es : succ h.size *)
    (* let children = fold (fun j f acc -> if f>0. then j::acc else acc) vt [] in *)
    let is = Internal { vt=empty_transitions ; ht=empty_transitions ; ilevel=succ(level h p) 
		      ; iparent=p ; iend = succ h.size ; children=[] } 
    and es = End {eparent=h.size ; elevel=succ(succ(level h p))} in
    let nh = add (add h is) es in
    add_vtransition nh p h.size pi, h.size 
     
  (* FIXME: if Array.length b <> h.card then raise (Hhmm "emission law does not have...") *)      
  let add_normal h ~p ~pi ~a ~b = 
    let ns = Normal {aqd = a ; bqd = b ; nparent = p ; nlevel=succ(level h p)} in
    add_vtransition (add h ns) p h.size pi,
    h.size

  let end_of h i = match Node.find i h.states with
  | Internal {iend=e} -> e
  | _ -> raise (Hhmm "not an internal state")
      
  (* FIXME: add the root or just its children (maybe an option)? *)
  let add_submodel h ~p ~pi g =
    assert (state h p = `i_state) ;
    let offset = h.size in
    let convp p = p + offset in
    let convc = List.map convp
    and convl l = l + level h p
    and convt t = Node.fold (fun j f acc -> Node.add (j + offset) f acc) t Node.empty in
    let nh = Node.fold (
      fun gi state acc ->
	assert (gi=acc.size-offset) ;
	(* eprintf "gi=%d\n" gi ; flush stderr ; *)
	let newstate = match state with
	| Internal {vt=gvt; ht=ght; iparent=p; ilevel=l ; iend=e ; children=children} -> 
	    Internal { vt=convt gvt ; ht=convt ght ; iparent=convp p 
		     ; ilevel=convl l ; iend=convp e ; children=convc children}
	| Normal {aqd=a ; bqd=b ; nparent=p ; nlevel=l} -> 
	    Normal {aqd=convt a ; bqd=b ; nparent=convp p ; nlevel=convl l}
	| End {eparent=p;elevel=l} -> 
	    End {eparent=convp p ; elevel=convl l} in
	if gi=root_state then add_vtransition (add acc newstate) p offset pi (* FIXME: check vtrans *)
	else add acc newstate
    ) g.states h in
    add_vtransition nh offset (end_of h p) 1.  (* FIXME: check vtrans *)

  let empty card = {states = Node.empty ; size=0 ; card=card}
  let root card = 
    add (
      add (empty card)
	(Internal { ht=empty_transitions
		  ; vt=empty_transitions
		  ; iparent=root_state
		  ; ilevel=0 
		  ; iend=1
		  ; children=[]
		  })
    ) (End {eparent=root_state ; elevel=1})

  let size h = h.size
  let card h = h.card

  let internal_h h i = match Node.find i h.states with
  | Internal {ht=ht} -> ht
  | _ -> raise (Hhmm (sprintf "state %d is not a internal state" i))

  let internal_v h i = match Node.find i h.states with
  | Internal {vt=vt} -> vt
  | _ -> raise (Hhmm (sprintf "state %d is not a internal state" i))

  let transition h i j = 
    let get t = try Node.find j t with _ -> 0. in
    if parent h j = i then
      match Node.find i h.states with
      | Internal {vt=vt} -> get vt
      | _ -> 0.    
    else if parent h i = parent h j then
      match Node.find i h.states with
      | Internal {ht=ht} -> get ht
      | Normal {aqd=a} -> get a
      | _ -> 0.
    else 0.

  let normal_a h i = match Node.find i h.states with
  | Normal {aqd=a} -> a
  | _ -> raise (Hhmm (sprintf "state %d is not a normal state" i))

  let normal_b h i = match Node.find i h.states with
  | Normal {bqd=b} -> b
  | _ -> raise (Hhmm (sprintf "state %d is not a normal state" i))

  (* could be out of I *)
  let has_intern_states h i =
    List.fold_left (fun acc j -> acc or state h j = `i_state) false (children h i)

  (* (unused) FIXME: check conditions *)      
  let lone_state h i = match Node.find i h.states with
  | Internal {vt=vt; ht=ht; iparent=p; ilevel=l ; iend=e} ->
      vt=empty_transitions && ht=empty_transitions && p=i && e=i && l=0 
  | Normal {aqd=a ; bqd=b ; nparent=p ; nlevel=l} ->
      a=empty_transitions && p=i && l=0
  | End {eparent=p;elevel=l} ->
      p=i && l=0

  (* TODO: modify algorithm to be able to convert intern states which have intern children *)
  let convert_intern_state h del =
    assert (del > 0) ;
    assert (not(has_intern_states h del)) ; 
    let newparent = parent h del 
    and delend = end_of h del in
    (* convert transitions possibly to deleted state *)
    let convin t = 
      if Node.mem del t then
	let p1 = Node.find del t in
	fold (fun j p2 acc -> Node.add j (p1*.p2) acc) (internal_v h del) (Node.remove del t)
      else t in
    (* convert transitions possibly to end state of deleted state *)
    let convout t =
      if Node.mem delend t then
	let p1 = Node.find delend t in
	fold (
	  fun j p2 acc -> 
	    (* there exists a self-transition for the deleted state *)
	    if j=del then fold (fun k p3 acc -> Node.add k (p1*.p2*.p3) acc) (internal_v h del) acc
	    else Node.add j (p1*.p2) acc
	) (internal_h h del) (Node.remove delend t)
      else t in
    let rec aux acc i =
      if i = h.size then acc
      else
	(* eprintf "convert_intern_state i=%d del=%d\n" i del ; flush stderr ; *)
	let newstate =
	  (* Case #1: deleted state and its end state 
	     makes non-connected states which could be further deleted
	  *)
	  if i=delend then 
	    End {eparent=i;elevel=0}
	  else if  i=del then
	    Internal {vt=empty_transitions;ht=empty_transitions;iparent=i;ilevel=0;iend=i;children=[]}
	  (* Case #2: states with possible transition to deleted state *)
	  else if parent h i<>del then
	    match Node.find i h.states with
	    | Internal state -> 
		if i=newparent then 
		  let nc = (children h del)@(List.filter (fun i -> i<>del && i<>delend) state.children) in
		  Internal { state with vt=convin state.vt ; ht=convin state.ht ; children=nc }  
		else Internal {state with vt=convin state.vt ; ht=convin state.ht}
	    | Normal state -> Normal {state with aqd=convin state.aqd}
	    | End e -> End e
	  (* Case #3: children of deleted state (i<>del && i<>delend && parent h i=del) *)	    
	  else 
	    match Node.find i h.states with
	    | Internal i -> failwith "should never happen, protected by assert"
	    | Normal {aqd=a ; bqd=b ; nparent=p ; nlevel=l} -> 
		Normal {aqd=convout a ; bqd=b ; nparent=newparent ; nlevel=pred l}
	    | End {eparent=p ; elevel=l} -> 
		End {eparent=newparent ; elevel=pred l}
	in aux (add acc newstate) (succ i)
    in
    aux (empty h.card) root_state

  open Generic
  let to_generic h = Base (CamlValue (Marshal.to_string h []))
  let of_generic = function
    | Base (CamlValue s) -> Marshal.from_string s 0
    | _ -> raise (Hhmm "generic value is not a HHMM")
end

open I

(** add a transition (determines automatically wether horizontal or vertical *)
let add_transition h i j =
  if parent h j = i then update_vtransition h i j
  else add_htransition h i j
  (*if state h i = `n_state or level h i = level h j then add_htransition h i j f
    else add_vtransition h i j f *)

(** add a list of transitions *)
let add_transitions = 
  List.fold_left (fun acc (i, j, f) -> add_transition acc i j f)

(** add a LR HMM as child of parent p *)
let add_chain h ~p ~pi ~ba =
  let l = Array.length ba
  and end_chain = end_of h p in
  assert (l>0) ;
  Myarray.fold_lefti (
    fun acc b ~i ->
      let nh, last = add_normal acc ~p ~pi:(if i=0 then pi else 0.) ~a:empty_transitions ~b 
      and next x = if i < pred l then succ x else end_chain in
      add_transitions nh [(last, last, 0.5); (last, next last, 0.5)]
  ) h ba  

(** determines the order of  the HHMM *)
let levelmax h =
  let rec aux m i =
    if i = size h then m
    else aux (max m (level h i)) (succ i)
  in
  aux (-1) root_state

(** convert any HHMM to an HHMM of order 1 *)
(* TODO: more efficient algorithm (make list of intern states, sort by level and convert) *)
let flatten h =
  let rec aux acc i =
    if i = size acc then acc
    else if state acc i = `i_state && not(I.has_intern_states acc i) then aux (I.convert_intern_state acc i) 1
    else aux acc (succ i)
  in
  aux h 1

let clist_to_string = Mylist.to_string (fun (a, b) -> sprintf "(%d,%d)" a b)
let to_hmm h =
  assert (not(I.has_intern_states h root_state)) ;  
  let states = List.sort compare (children h root_state) in
  let nbs = List.length states   
  (* TODO: inefficient: use map or array for index *)
  and index = snd (List.fold_left (fun (n, acc) i -> succ n, (i, n)::acc) (0, []) states) in
  (* let pi = fold (fun _ f acc -> f::acc) (internal_v h root_state) [] in *)
  eprintf "to_hmm: (nbs=%d)\n.index=%s\n.states=%s\n" nbs 
    (clist_to_string index) (Mylist.to_string string_of_int states); flush stderr ; 
  let conva i = 
    eprintf "conva(i=%d): " i ; flush stderr ;
    let r = Array.make nbs 0. 
    and rend = ref 0. in
    I.iter (fun j f -> 
	      eprintf "[j=%d] " j ; flush stderr ;
	      if state h j = `n_state then r.(List.assoc j index) <- f
	      else rend := f
	   ) (normal_a h i) ;
    eprintf "\n%s\n" (Myarray.show ~name:(sprintf "r(%d)" i) string_of_float r) ; flush stderr ;
    r, !rend in
  let al, aendl = List.split (List.map conva states) in  
  let st = Array.of_list (List.map (fun i -> Some (sprintf "q_%d" i)) states)
  and pi = Array.of_list (List.map (fun j -> transition h root_state j) states)
  and a = Array.of_list al
  and aend = Array.of_list aendl
  and b = Array.of_list (List.map (normal_b h) states) in
  Hmm.D.make ~st ~pi ~a ~b ~aend ()

let likelihood h =
  let hmm = to_hmm (flatten h) in
  fun x -> snd (Hmm.D.forward hmm x)

(** convert HHMM to dot language for output
    inefficient: Set of Maps for level, only one fold...
    only one option for now: `nofloats does not display transitions probabilities 
    (helps because of dot bugs with constrained ranks) *)
let to_dot options h =
  eprintf "to_dot:size=%d,levelmax=%d\n" (size h) (levelmax h) ; flush stderr ;
  let style = function
    | `i_state -> "style=filled,color=\".7 .3 1.0\""
    | `n_state -> ""
    | `e_state -> "style=filled,color=\".5 .5 .5\"" 
  and tstyle = function
    | 1. -> ""
    | f -> sprintf "label=\"%s\",color=\"#886666\",fontcolor=\"#886666\"" 
	(if List.mem `trans options then Mystring.of_float f else "")
  in
  let names = Array.init (size h) (fun i -> sprintf "q^%d_%d" (level h i) i)
  and styles = Array.init (size h) (fun i -> style (state h i)) in
  (** transitions *)
  let trans = 
    let rec aux acc i = 
      if i < size h then aux (
	match state h i with
	| `i_state -> 
	    fold (fun j f acc -> if f > 0. then (i, j, f, tstyle f)::acc else acc) (internal_v h i)
	      (fold (fun j f acc -> if f > 0. then (i, j, f, tstyle f)::acc else acc) (internal_h h i) acc)
	| `n_state -> 
	    fold (fun j f acc -> if f > 0. then (i, j, f, tstyle f)::acc else acc) (normal_a h i) acc
	| `e_state -> 
	    (i, parent h i, 1., tstyle 1.)::acc
      ) (succ i)
      else acc
    in aux [] root_state 
  in
  let levels = 
    let rec aux l acc i =
      if i = size h then acc
      else aux l (if level h i = l then i::acc else acc) (succ i) in
    Array.init (succ (levelmax h)) (fun l -> aux l [] root_state)
  in
  (* FIXME: rank=same should be an option *)
  "digraph Hhmm {\n" ^ (
    Myarray.fold_lefti (
      fun acc level ~i -> 
	acc ^ (sprintf "  { rank=same ;\n" (*%d i*)) ^ (
	  List.fold_left (
	    fun acc i ->
	      if i=root_state or parent h i<>i then (* FIXME: non-connected states *)
		acc ^ (sprintf "    node%d [label=\"%s\",%s] ;\n" i names.(i) styles.(i))
	      else acc
	  ) "" level
	) ^ "  }\n"
    ) "" levels
  ) ^ (
    List.fold_left (
      fun acc (i, j, _, ts) -> 
	if i=root_state or parent h i<>i then (* FIXME: non-connected states *)
	  acc ^ sprintf "  node%d -> node%d [%s] ;\n" i j ts
	else acc
    ) "" trans
  ) ^ "}\n"
