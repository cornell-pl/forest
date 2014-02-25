(* Copyright (C) 2001-2003 Henri Binsztok
   
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

(** dynamic typing with string types (prototype)
    @author Henri Binsztok 2001-2003
    this portion of DT code is released under the LGPL license
*)

exception Bad_type of string
exception Mixed_types
exception Empty_iter

type uniqueid = int
type file = string (* filename *)
type replicate = uniqueid
type localref = uniqueid
type label = uniqueid (* maybe more constraint *)
type labelname = string
type typename = string

type t_base =
  | Empty
  | Int of int
  | Float of float
  | Char of char
  | String of string
  | Bool of bool
  | Xml of string
  | Dtd of string
  | Label of string (* FIXME: très mal nommé *)
  | Type of string (* user defined type *)
  | Date of int (** days from 1/1/70 *)
  | Time of int (** seconds from 12pm *)
  | Blob of file (** file *)
  | Weak of replicate * localref
  | CamlValue of string

type t_data =
  | Group
  | Base of t_base
  | Record of t_data array
  | Variant of label * t_data

let length = function
  | Record a -> Array.length a
  | _ -> raise (Bad_type "length: not a record")

let fold_left f i = function
  | Variant (_, d) -> f i d
  | Record a -> Array.fold_left f i a
  | _ -> raise (Bad_type "fold_left only on record (and variant)")
  
let map f = function
  | Variant (v, d) -> [| f d |]
  | Record a -> Array.map f a
  | _ -> raise (Bad_type "map only on record (and variant)")

let iter f = function
  | Variant (_, d) -> f d
  | Record a -> Array.iter f a
  | _ -> raise (Bad_type "iter only on record (and variant)")

(* constructors *)

let option f d = match d with None -> Base Empty | Some d -> f d
let unit _ = Base Empty
let int i = Base (Int i)
let float f = Base (Float f)
let char c = Base (Char c)
let string s = Base (String s)
let bool b = Base (Bool b)
let weak ?(r=0) e = Base (Weak (r, e))
let value a = Base (CamlValue (Marshal.to_string a []))
let list f l = Record (Array.map f (Array.of_list l))
let array f a = Record (Array.map f a)
let couple f1 f2 (a, b) = Record [| f1 a ; f2 b |]
let triplet f1 f2 f3 (a, b, c) = Record [| f1 a ; f2 b ; f3 c |]
(** converts a hashtable to generic data
  @param cnvf value conversion function
  @param labf key to label function
  @todo faster ? 
  @FIXME: inverser l'ordre des paramètres (plus cohérent)
*)
let hashtbl cnvf labf h = 
  let l = Myhash.fold_sort ( (* order MATTERS *)
    fun x y acc ->
      (x, y) :: acc
  ) h [] in
  let a = Array.of_list l in
  Record (Array.map (fun (x, y) -> Variant (labf x, cnvf y)) a)

let type_of_base b =
  match b with
  | Empty -> "empty"
  | Int i -> "int"
  | Float f -> "float"
  | Char c -> "char"
  | String s -> "string"
  | Bool b -> "bool"
  | Xml s -> "xml"
  | Dtd s -> "dtd"
  | Label s -> "label"
  | Date i -> "date"
  | Time i -> "time"
  | Blob s -> "blob"
  | Weak (r,l) -> "weak"
  | Type t -> "type"
  | CamlValue v -> "ocaml value"

let rec type_of d =
  match d with
  | Group -> "group"
  | Base b -> (* "base " ^ *) type_of_base b
  | Record [||] -> "empty record"
  | Record r -> 
      let content = Array.map type_of r in
      if Myarray.is_same content then content.(0) ^ " record"
      else (
	Myarray.fold_lefti (
	  fun acc x ~i ->
	    acc ^ type_of x ^ if i < Array.length r - 1 then "," else ""
	) "["  r
      ) ^ ("]") (* FIXME: "record ]" *)
  | Variant (v, d) -> Printf.sprintf "(%d:%s)" v (type_of d)

let bad_type d = raise (Bad_type (type_of d))

let in_variant d = match d with Variant (_, d) -> d | _ -> bad_type d

let value_of d = match d with Base (CamlValue v) -> Marshal.from_string v | _ -> bad_type d
let option_of f d = match d with Base Empty -> None | d -> Some (f d)
let bool_of d = match d with (Base (Bool b)) -> b | _ -> bad_type d
let int_of d = match d with (Base (Int i)) -> i | _ -> bad_type d
let float_of d = match d with (Base (Float f)) -> f | _ -> bad_type d
let char_of d = match d with (Base (Char c)) -> c | _ -> bad_type d
let string_of d = match d with (Base (String s)) -> s | _ -> bad_type d
let label_of d = match d with (Base (Label l)) -> l | _ -> bad_type d
let couple_of f1 f2 d = match d with Record [| a ; b |] -> (f1 a, f2 b) | _ -> bad_type d
let triplet_of f1 f2 f3 d =
  match d with Record [| a ; b ; c |] -> 
    (f1 a, f2 b, f3 c) | _ -> bad_type d
let list_of f d = 
  match d with (Record r) -> 
    Array.fold_right (fun x acc -> (f x) :: acc) r []
  | _ -> bad_type d
let array_of f d = 
  match d with (Record r) -> Array.map f r
  | _ -> bad_type d
let hashtbl_of labf f d = 
  match d with (Record r) ->
    let h = Hashtbl.create 0 in
    Array.iter (
      function
	  (Variant (l, x)) -> Hashtbl.add h (labf l) (f x)
	| _ -> () (* ignores non-variants *)
    ) r ; h
  | _ -> bad_type d
