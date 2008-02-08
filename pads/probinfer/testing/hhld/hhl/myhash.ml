(* Copyright (C) 2000-2005 Henri Binsztok
   
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

(** extensions to Hashtbl module
  @author Henri Binsztok 2000-2005
*)
open Hashtbl

(* added to ocaml!
(** length of hash *)
   let length h = fold (fun _ _ acc -> acc + 1) h 0
*)

(** iter on hash, keys taken in sorted order *)
let iter_sort f h =
  let l = List.sort compare (
    fold (fun x _ a -> x :: a) h []
  ) in
    List.iter (
      fun k ->
	f k (find h k)
    ) l

(** iter with i *)
let iteri f h =
  ignore (
    fold (
      fun x y acc ->
	f x y acc ; succ acc
    ) h 0
  )

(** fold with i *)
let foldi f h init =
  fst (
    fold (
      fun x y (acc, i) ->
	(f x y acc i, i + 1)
    ) h (init, 0)
  )

(** fold on hash, keys taken in sorted order *)
let fold_sort f h i =
  let l = List.sort compare (
    fold (fun x _ a -> x :: a) h []
  ) in
    List.fold_left (
      fun acc k ->
	f k (find h k) acc
    ) i l

(** keys presents only in first hash *)
let only_first h1 h2 =
  fold (
    fun x _ acc ->
      if mem h2 x then acc else x :: acc
  ) h1 []

(** extracts the sub-hash specified by its key list 
  @warning may raise Not_found
*)
let extract h l =
  let r = create (List.length l) in
    List.iter (fun x -> add r x (find h x)) l ;
    r
	
let keys h = fold (fun x _ acc -> x :: acc) h []
let values h = fold (fun _ y acc -> y :: acc) h []

let add_replace h k v = (if mem h k then replace else add) h k v
let add_ifnot h k v = if not (mem h k) then add h k v

(** creates an inverted index on the hash: provided the hash is not modified,
  it returns a unique int between 0 .. length - 1 that is in bijection with the key
*)
let index h =
  let idx = create 0 in
  iteri (fun x _ i -> add idx x i) h ;
  fun k -> find idx k

(** first element of a hash *)
let first h =
  let fk = List.hd (keys h) in
  (fk, find h fk)    

let arg_oper op h = 
  fst (
    fold (
      fun x y (mk, mv) ->
	if op y mv then (x, y) else (mk, mv)
    ) h (first h)
  )

let argmax a = arg_oper (>) a
let argmin a = arg_oper (<) a

(** returns a random (key, value) *)
let random h = 
  let n = Random.int (length h) in
  foldi (fun x y acc i -> if i = n then (x, y) else acc) h (first h)

(** split randomly a hash into two pieces
  @FIXME this souldn't exist, since this should return two lists of keys, not touching the hash itself
*)
let random_split h =
  let h1 = create 0
  and h2 = create 0 in
  iter (fun x y -> add (if Random.int 2 = 0 then h1 else h2) x y) h ;
  h1, h2

(** double map on keys and values *)
let dmap fk fv h =
  let r = create 0 in iter (fun k v -> add r (fk k) (fv v)) h ; r

(** convert to a sorted list of couples (k,v) list) *)
let to_list h =
  List.rev (
    fold_sort (
      fun x y acc ->
	(x, y) :: acc
    ) h []
  )

(** convert a (key,value) list to hash *)
let of_list l =
  let h = create (List.length l) in
  List.iter (fun (x, y) -> add h x y) l ; h

(** create a hash for fast search in 'a list *)
let of_list1 l =
  let h = create (List.length l) in
  List.iter (fun x -> add h x true) l ; h

(** add one to a count as hashtable *)
let count h k = if mem h k then let ov = find h k in replace h k (ov + 1) else add h k 1

(** add all keys keys from h1 to h *)
let add_hash h h1 =
  Hashtbl.iter (
    fun x y ->
      Hashtbl.add h x y
  ) h1
