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

(** Functions on matrix
    @author Henri Binsztok 2000-2005
    @todo factor code for add_value, mul, like prod...
*)

open Array

(** unity matrix of size d*d *)
let unity d =
  init d (fun i -> init d (fun j -> if i=j then 1. else 0.))

(** adds a value to each coefficient (in place) *)
let add_value m v =
  for i = 0 to pred (length m) do
    for j = 0 to pred (length m.(i)) do
      m.(i).(j) <- m.(i).(j) +. v
    done 
  done

(** multiply each coefficient by a value (in place) *)
let mul m v =
  for i = 0 to pred (length m) do
    for j = 0 to pred (length m.(i)) do
      m.(i).(j) <- m.(i).(j) *. v
    done 
  done

(** somme de tous les éléments d'une matrice *)
let sum =
  fold_left (fun acc a -> acc +. fold_left (+.) 0. a) 0.

(** produit de deux matrices (coef à coef) *)
let prod m1 m2 =
  let n = length m1 and m = length m1.(0) in
  assert (n = length m2 && m = length m2.(0)) ;  
  init n (fun i -> init m (fun j -> m1.(i).(j) *. m2.(i).(j)))

(** map on a matrix *)
let mmap f = map (map f)

(** symétrise la matrice (calcul en place) *)
let symetrize mat =
  let n = length mat in
  assert (n = length mat.(0)) ;
  for i = 0 to n - 1 do
    mat.(i).(i) <- 2. *. mat.(i).(i) ;
    for j = i + 1 to n - 1 do
      mat.(i).(j) <- mat.(i).(j) +. mat.(j).(i) ;
      mat.(j).(i) <- mat.(i).(j)      
    done 
  done;;

(** normalize each second dimension *)
let normalize ?(smooth=false) m =
  let n = length m in
  init n (fun i -> Myarray.normalize ~smooth m.(i)) ;;

(** argmin or argmax of matrix 
    @todo opt (first) + rewrite
*)
let arg op m =
  let ax = ref 0 and ay = ref 0 in
  for i = 1 to pred (length m) do
    for j = 1 to pred (length m.(i)) do
      if op m.(i).(j) m.(!ax).(!ay) then (
	ax := i ; ay := j
      )
    done
  done ;
  (!ax, !ay)

(** maximum of matrix *)
let max m = let i, j = arg (>) m in m.(i).(j)

(** minimum of matrix *)
let min m = let i, j = arg (<) m in m.(i).(j)

(** max even if nan in values *)
let max_nan m =
  let f a b = match a, b with
    | a, _ when Maths.is_nan a -> false 
    | _, b when Maths.is_nan b -> true
    | _ -> a > b in
  let i, j = arg f m in m.(i).(j)

(** transpose matrix *)
let transpose m =
  let x = Array.length m
  and y = Array.length m.(0) in
  Array.init y (fun a -> Array.init x (fun b -> m.(b).(a)))

open Printf
(* FIXME: functional *)
let to_string ?(csv=false) ?(name="") m =
  let r = ref (
    if csv then ""
    else sprintf "MAT %s n=%d m=%d\n" name (Array.length m) (Array.length m.(0))
  ) in
  for i = 0 to pred (Array.length m) do
    for j = 0 to pred (Array.length m.(i)) do
      r := !r ^ (if csv then sprintf "%.4f," m.(i).(j) else sprintf "%.4f " m.(i).(j))
    done ;
    r := !r ^ "\n"
  done ;
  !r

(** export to latex tabular *)
let to_latex ~f m =
  let x = Array.length m
  and y = Array.length m.(0) in  
  let r = ref (sprintf "\\begin{tabular}{|%s}\n\\hline\n" (Mystring.repeat "l|" y)) in
  for i = 0 to pred x do
    for j = 0 to pred y do
      r := !r ^ f m.(i).(j) ^ if j < pred y then "&" else ""
    done ;
    r := !r ^ "\\\\\n\\hline\n" ;
  done ;
  !r ^ "\\end{tabular}"
    
let float_to_latex = to_latex ~f:string_of_float
let int_to_latex = to_latex ~f:string_of_int

(** reads a float matrix from a file *)
let read f =
  let ff = open_in f in
  let size = Str.split (Str.regexp " ") (input_line ff) in 
  let r = match size with
    [x ; y] ->
      let xx = int_of_string x and yy = int_of_string y in
      let m = Array.create_matrix xx yy 0. in
      for i = 0 to xx - 1 do
	let l = Str.split (Str.regexp " ") (input_line ff) in
	ignore( 
	  List.fold_left (
	    fun acc x ->
	      if acc < yy then m.(i).(acc) <- float_of_string x ;
	      succ acc
	  ) 0 l
	)
      done ;
      m      
  | _ -> failwith "first line in matrix does not begin with size" in
  close_in ff ;
  r

(** matrix is not empty and all arrays are of same length 
    matrix should be a dependant type / this function should then no more exist...
*)
let valid x =
  length x > 0 &&
    let d = length x.(0) in
    Array.fold_left (fun acc y -> acc && d = length y) true x
