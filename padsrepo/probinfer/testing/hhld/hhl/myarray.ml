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

(** extensions to Array module
    @author Henri Binsztok 2000-2005
*)

open Array

exception Empty

let to_list_some a =
  fold_left (
    fun acc x ->
      match x with
	None -> acc
      | Some x -> x :: acc
  ) [] a

let to_list_notempty a =
  fold_left (
    fun acc x ->
      match x with
	[] -> acc
      | _ -> x :: acc
  ) [] a

let some a = of_list (to_list_some a)
let make_some a = map (fun x -> Some x) a
let notempty a = of_list (to_list_notempty a)

let iter_some f =
  iter (
    fun x ->
      match x with
	Some x -> f x 
      | None -> ()
  )

let fold_left_some f =
  fold_left (
    fun acc x ->
      match x with
	Some x -> f acc x 
      | None -> acc
  )

let map_some f = Array.map (function None -> None | Some x -> Some (f x))

let fold_lefti f d a =
  snd (
    fold_left (
      fun (i, acc) x -> succ i, f acc x ~i
    ) (0, d) a
  )

let oper op a =
  let r = ref a.(0) in
  for i = 1 to length a - 1 do
    r := op !r a.(i)
  done ;
  !r   

let arg_oper op a = 
  let m = ref 0 in
  for i = 1 to length a - 1 do
    if op a.(i) a.(!m) then m := i
  done ;
  !m

let max a = oper Pervasives.max a
let min a = oper Pervasives.min a
let sum = oper (+.)
let prod = oper ( *.)
let argmax a = arg_oper (>) a
let argmin a = arg_oper (<) a

let show ?(name="") f a =
  let r = ref (Printf.sprintf "%s:(%d) = [|" name (length a)) in
  for i = 0 to pred (length a) do
    r := !r ^ (if i>0 then "; " else "") ^ f a.(i)
  done ;
  !r ^ "|]"

(** true if all elements in array are equal *)
let is_same a =
  match length a with
    0 -> true
  | _ -> fst (
      fold_left (
	fun (acc, p) x ->
	  acc & p = x, x
      ) (true, a.(0)) a
    )

(** Compute a Pervasives.compare index on array
  index [| "toto"; "tji"; "abc"; "der" |] = [|2; 3; 1; 0|]
*)
let index a =
  let rec indices n = 
    match n with 
      0 -> []
    | n -> (n - 1) :: indices (n - 1) in  
  let l = indices (length a) in
  let s = List.sort (fun i j -> compare a.(i) a.(j)) l in
  of_list s

(** equivalent for arrays to let hd::tl = l in List.fold_left f hd tl *)
let fold_ht f a =
  let n = length a in
  if n = 0 then raise Empty
  else 
    let rec aux i =
      if i = 0 then a.(0)
      else f (aux (pred i)) a.(i) in
    aux (pred n)

(** remove cell k from array, returns fresh array of length n - 1 *)
let remove a k =
  init (pred (length a)) (fun i -> a.(if i < k then i else succ i))

(** last element *)
let last a = a.(pred (length a))

(** find optimal for fun f, return argmax, max *)
let find_opt op f a =
  let r = ref 0 
  and c = ref (f a.(0)) in
  for i = 1 to length a - 1 do     
    let t = f a.(i) in if op t !c then (r := i ; c := t)
  done ;
  !r, !c

(** randomly partition in n arrays *)
type 'a partition_tree = Empty | Node of 'a partition_tree * 'a partition_tree | Leaf of 'a
let rec build_tree = function
    [] -> failwith "empty"
  | [v] -> Leaf v
  | l -> let l1, l2 = Mylist.split_in_two l in Node (build_tree l1, build_tree l2)

(** normalize vector *)
let normalize ?(smooth=false) a =
  let s = fold_left (+.) 0. a 
  and l = float_of_int (length a) in
  if s=0. then a else (* FIXME: or raise Null_vector ? *)
    Array.map (fun x -> if smooth then (x +. 1.) /. (s +. l) else x /. s) a
