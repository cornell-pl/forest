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

(** extensions to List module
    @author Henri Binsztok 2000-2005
*)

open List

exception Empty

let remove v = filter (fun x -> x<>v)

(** smart cons *)
let cons e l = 
  match e with
  | [] -> l
  | [e] -> e::l
  | l2 -> l2 @ l
let (@@) = cons

let map3 f g al bl cl =
  map2 g (map2 f al bl) cl

let oper op l =
  match l with
    [] -> raise Empty
  | hd :: tl ->
      fold_left (
	fun acc x ->
	  op acc x
      ) hd tl

let max l = oper max l
let min l = oper min l
let prod l = fold_left ( *.) 1. l
let sum l = fold_left (+.) 0. l

let average = function
  | [] -> raise Empty
  | hd::tl -> 
      let len, sum = fold_left (
	fun (len, sum) x -> (len +. 1., sum +. x)
      ) (1., hd) tl in sum /. len

let fill n v =
  let rec addone l n =
    if n > 0 then addone (v::l) (n - 1) else l 
  in    
  addone [] n

(** alike Array.iteri, but beware order of parameters is changed... *)
let iteri f l =
  ignore (
    fold_left (
      fun i x ->
	ignore (f x ~i);
	succ i
    ) 0 l
  )

(** like List.fold_left but f acc x i *)
let fold_lefti f d l =
  snd (
    fold_left (
      fun (i, acc) x -> i + 1, f acc x i
    ) (0, d) l
  )

let rev_mapi f l = fold_lefti (fun acc x i -> f x i :: acc) [] l
let mapi f l = rev (rev_mapi f l)

let list1 l = fst (split l)
let list2 l = snd (split l)

(** random split of list in two: could be replaced by heuristics depending on data *)
let random_split l = 
  fold_left (
    fun (l1, l2) x ->
      if Random.int 2 = 0 then (x :: l1, l2)
      else (l1, x :: l2)
  ) ([], []) l

(** true if all elements in list are equal *)
let is_same l =
  match l with
    [] -> true
  | hd::tl -> fst (
      fold_left (
	fun (acc, p) x ->
	  acc & p = x, x
      ) (true, hd) tl
    )

(** replace all occurrences of x by y *)
let change x y = fold_left (fun acc z -> if z = x then y :: acc else z :: acc) []

(** convert (x, y) list to (y, x) list *)
let invert l = fold_left (fun acc (x, y) -> (y, x) :: acc) [] l

(** fold_left with init head and fold on tail *)
let fold_ht f = function
    [] -> failwith "mylist::fold_ht: empty list"
  | hd :: tl -> fold_left f hd tl

(** equivalent to Array.make *)
let rec make n v = match n with 0 -> [] | _ -> v :: (make (n - 1) v)

(** returns a list of integers from 0 (included) to n (excluded) *)
let indices n = 
  let rec indices i =
    if i < n then i :: indices (i + 1) else [] in
  indices 0

(** reversed indices *)
let rec rev_indices n =
  if n = 0 then [] else (n - 1) :: (rev_indices (n - 1))

(** equivalent to Array.init *)
let init n f = fold_left (fun acc x -> f x :: acc) [] (rev_indices n)

(** split a list in two lists of same length (or +1 for first if even length)
  if length > 1 then neither list is empty *)
let split_in_two l =
  let rec split (g, d) b = function
      [] -> (g, d)
    | hd::tl -> split (if b then (hd::g, d) else (g, hd::d)) (not b) tl in
  split ([], []) true l

open Printf
let crochet s = sprintf "[%s]" s
let to_string f l = crochet (
  match l with
  | [] -> ""
  | hd::tl -> List.fold_left (fun acc x -> acc ^ "; " ^ f x) (f hd) tl
)
