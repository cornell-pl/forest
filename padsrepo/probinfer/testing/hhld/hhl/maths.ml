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

(** mathematical functions
    @author Henri Binsztok 2000-2005 *)

let pi = acos (-1.)
 
let rad_of_deg a = a *. (2. *. pi /. 360.)

let is_nan x = not (x = x)

(** float is int ? *) (* FIXME: better impl. *)
let is_int f = f = float_of_int (int_of_float f)

(** compute a sum of logarithms, with scaling *)
let logsum l =
  let m = Myarray.max l in
  let sigma = Array.fold_left (
    fun acc x ->
      acc +. (exp (x -. m))
  ) 0. l in
  log sigma +. log m

(** generate a symbol randomly according to prob dist in float array *)
let which p =
  let r = Random.float 1. 
  and p = Myarray.normalize p in
  snd (
    Myarray.fold_lefti (
      fun (s, w) x ~i ->
	if w < 0 then
	  let s = s +. x in
	  if r < s then (-1., i) else (s, (-1))
	else (s, w)
    ) (0., (-1)) p 
  )

(** round value *)
let round ?(eps=0.001) f =
  match f with
    f when abs_float (f -. (ceil f)) < eps -> ceil f
  | f when abs_float (f -. (floor f)) < eps -> floor f
  | f -> f	    

(** Logistic function
  @see <http://mathworld.wolfram.com/LogisticDistribution.html> 
*)
let logistic ~m ~b x = 1. /. (1. +.  exp ((m -. x) /. b))

(** compute \sum_i=0^n f(i) *)
let sum ~f ~n =
  let rec sum i r =
    if i < n then sum (succ i) (r +. f i)
    else r
  in sum 0 0.

let sum_int ~f ~n =
  let rec sum i r =
    if i < n then sum (succ i) (r + f i)
    else r
  in sum 0 0

(** max and argmax for f(i) with i=0..n-1 *)
let max_and_arg ~f ~n =
  let rec aux i m am =
    if i < n then
      let tmp = f i in
      if tmp > m then aux (succ i) tmp i
      else aux (succ i) m am
    else (m, am) in
  aux 0 neg_infinity (-1)
