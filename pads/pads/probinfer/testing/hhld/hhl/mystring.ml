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

(** extensions to String module
  @author Henri Binsztok 2000-2005
*)

open String

(** del first char of string *)
let del_first s = sub s 1 (pred (length s))

(** del last char of string *)
let del_last s = sub s 0 (pred (length s))

(** returns last char *)
let last_char s = get s (pred (length s))

(** map *)
let map f s =
  let r = copy s in
  for i = 0 to pred (length s) do
    r.[i] <- f s.[i]
  done ;
  r

let rev_map f s =
  let l = length s 
  and r = copy s in
  for i = 0 to pred (length s) do
    r.[l-i-1] <- f s.[i]
  done ;
  r

(** remove ending with \r from Windows files *)
let remove_windows s =
  let l = pred (length s) in
  if s.[l] = '\r' then sub s 0 l else s

(** fold on a string *)
let fold f i s =
  let r = ref i in
  iter (fun x -> r := f !r x) s ;
  !r

(** generate a hash (weak) from a string 
    FIXME: implement MD4 or MD5 or other algorithm
*)
let hash = 
  fold (fun acc x -> let x = int_of_char x in acc + ((x * x) lxor acc)) 24

(** rev fold *)
let fold_rev f i s =
  let acc = ref i in
  for k = length s - 1 downto 0 do
    acc := f !acc s.[k]
  done ;
  !acc
    
(** repeats a string a number of times *)
let repeat s n =
  let rec iter nb r =
    if nb <= 0 then r else iter (pred nb) (r ^ s) in
  iter n ""

(** converts a string to char list 
    @warning not tail-recursive *)
let to_chars s =
  let r = ref [] in
  iter (
    fun x -> r := x :: !r
  ) s;
  List.rev !r

(** converts a char list to a string *)
let of_chars =
  List.fold_left (
    fun acc x ->
      acc ^ (Char.escaped x)
  ) ""

(** nice string_of_float : .5 -> "1/2" 
    @FIXME: 
    BUGS: 1. -> 2/2
    better algorithm, utiliser les nombres premiers, ajouter les 2 sqrt 2, 2pi, pi/2, ...
    utiliser le fait plutôt que f*f "entier", etc...
    si entier, enlever le trailing dot...
*)
open Printf
let of_float f =
  (* FIXME: utiliser Maths.pi *)
  if Maths.is_int f then string_of_int (int_of_float f)
  else if f = acos (-1.) then "pi"
  else if f = exp 1. then "e"
  else 
    let r = ref "" in
    for i = 9 downto 1 do
      for j = 10 downto 2 do
	if f = float_of_int i /. float_of_int j then r := sprintf "%d/%d" i j
      done
    done ;
    if !r = "" then
      (* FIXME: marquer la racine plus jolie... en unicode ? *)
      List.iter (fun x -> if f = sqrt(float_of_int x) then r := sprintf "V%d" x) [2;3;5;7] ;
    (* FIXME: paramètrer le formattage du float par défaut *)
    if !r = "" then sprintf "%.2f" f else !r
