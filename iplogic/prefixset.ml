(* Copyright (C) 2012  Petter Urkedal <paurkedal@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Pervasive

let get_bit n x = (x lsr n) land 1 = 1

let lowmasked n x = x land ((1 lsl n) - 1)

type t =
   | Bot | Top
   | Y of t * t
   | P of Bitstring.t * t

(* Canonicalizing variant of P. *)
let rec cP p s =
    if Bitstring.length p = 0 then s else
    match s with
      | Bot -> Bot
      | P (p', s') -> P (Bitstring.cat p p', s')
      | _ -> P (p, s)

(* Canonicalizing variant of Y. *)
let cY sL sR = match sL, sR with
  | Top, Top -> Top
  | Bot, Bot -> Bot
  | _, Bot -> cP (Bitstring.init 1 (konst false)) sL
  | Bot, _ -> cP (Bitstring.init 1 (konst true)) sR
  | _ -> Y (sL, sR)

let rec valid = function
  | Top -> true
  | Bot -> true
  | Y (Bot, Bot) -> false
  | Y (Top, Top) -> false
  | Y (s0, s1) -> valid s0 && valid s1
  | P (p, Bot) -> false
  | P (p, P _) -> false
  | P (p, s) -> Bitstring.length p > 0 && valid s

let rec equal sL sR = match sL, sR with
  | Top, Top -> true
  | Bot, Bot -> true
  | Y (sL0, sL1), Y (sR0, sR1) -> equal sL0 sR0 && equal sL1 sR1
  | P (pL, sL0), P (pR, sR0) -> Bitstring.equal pL pR && equal sL0 sR0
  | Top, _ | Bot, _ | Y _, _ | P _, _ -> false

let empty = Bot
let is_empty = function Bot -> true | _ -> false

let universe = Top
let is_universe = function Top -> true | _ -> false

let of_prefix p =
    if Bitstring.length p = 0 then Top else
    P (p, Top)
let is_prefix = function
  | Top -> true
  | P (p, Top) -> true
  | _ -> false
let to_prefix = function
  | Top -> Bitstring.empty
  | P (p, Top) -> p
  | _ -> invalid_arg "Bitstring.to_prefix: Not a prefix."

let rec follow pG s =
    let nG = Bitstring.length pG in
    let rec loop iG s =
	if iG = nG then s else
	match s with
	  | Bot -> Bot
	  | Top -> Top
	  | Y (s0, s1) ->
	    loop (iG + 1) (if Bitstring.get iG pG then s1 else s0)
	  | P (p0, s0) ->
	    let n0 = Bitstring.length p0 in
	    let n = Bitstring.common_prefix_length_from iG pG 0 p0 in
	    if n = n0 then loop (iG + n) s0 else
	    if n = nG - iG then P (Bitstring.slice n n0 p0, s0) else
	    Bot in
    loop 0 s

let rec modify pG f s =
    let nG = Bitstring.length pG in
    let rec loop iG s =
	if iG = nG then f s else
	match s with
	  | Bot | Top ->
	    if Bitstring.get iG pG then cY s (loop (iG + 1) s)
				   else cY (loop (iG + 1) s) s
	  | Y (s0, s1) ->
	    if Bitstring.get iG pG then cY s0 (loop (iG + 1) s1)
				   else cY (loop (iG + 1) s0) s1
	  | P (p0, s0) ->
	    let n0 = Bitstring.length p0 in
	    let n = Bitstring.common_prefix_length_from iG pG 0 p0 in
	    if n = n0 then cP p0 (loop (iG + n) s0) else
	    let s_new =
		if iG + n = nG then f (P (Bitstring.slice n n0 p0, s0)) else
		let s_unm = cP (Bitstring.slice (n + 1) n0 p0) s0 in
		let s_mod = cP (Bitstring.slice (iG + n + 1) nG pG) (f Bot) in
		if Bitstring.get n p0 then (cY s_mod s_unm)
				      else (cY s_unm s_mod) in
	    cP (Bitstring.prefix n p0) s_new in
    loop 0 s

let add p = modify p (konst Top)
let remove p = modify p (konst Bot)

let fold_prefixes f =
    let rec loop p = function
      | Bot -> ident
      | Top -> f p
      | Y (s0, s1) -> loop (Bitstring.cat p (Bitstring.singleton true)) s1
		   |< loop (Bitstring.cat p (Bitstring.singleton false)) s0
      | P (p0, s0) -> loop (Bitstring.cat p p0) s0 in
    loop Bitstring.empty

let iter_prefixes f s = fold_prefixes (fun x () -> f x) s ()

let rec card_prefixes = function
  | Bot -> 0
  | Top -> 1
  | Y (s0, s1) -> card_prefixes s0 + card_prefixes s1
  | P (p, s) -> card_prefixes s

let rec isecn sL sR = match sL, sR with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Top, _ -> sR
  | _, Top -> sL
  | Y (sL0, sL1), Y (sR0, sR1) -> cY (isecn sL0 sR0) (isecn sL1 sR1)
  | P (pL, sL), _ ->
    begin match follow pL sR with
    | Bot -> Bot
    | sR' -> cP pL (isecn sL sR')
    end
  | _, P (pR, sR) ->
    begin match follow pR sL with
    | Bot -> Bot
    | sL' -> cP pR (isecn sL' sR)
    end

let rec union sL sR = match sL, sR with
  | Bot, _ -> sR
  | _, Bot -> sL
  | Top, _ -> Top
  | _, Top -> Top
  | Y (sL0, sL1), Y (sR0, sR1) -> cY (union sL0 sR0) (union sL1 sR1)
  | P (pL, sL), _ ->
    modify pL (fun sR0 -> union sL sR0) sR
  | _, P (pR, sR) ->
    modify pR (fun sL0 -> union sL0 sR) sL

let rec abs_compl = function
  | Bot -> Top
  | Top -> Bot
  | Y (s0, s1) -> cY (abs_compl s0) (abs_compl s1)
  | P (p0, s0) ->
    let n0 = Bitstring.length p0 in
    let rec loop i0 =
	if i0 = n0 then abs_compl s0 else
	if Bitstring.get i0 p0 then cY Top (loop (i0 + 1))
			       else cY (loop (i0 + 1)) Top in
    loop 0

let rec compl sL sR = match sL, sR with
  | Bot, _ | _, Top -> Bot
  | _, Bot -> sL
  | Top, _ -> abs_compl sR
  | Y (sL0, sL1), Y (sR0, sR1) -> cY (compl sL0 sR0) (compl sL1 sR1)
  | P (pL, sL), _ -> cP pL (compl sL (follow pL sR))
  | _, P (pR0, sR0) -> modify pR0 (fun sL0 -> compl sL0 sR0) sL

let rec compl_decomp = function
  | Bot -> Bot, Bot
  | Top -> Top, Bot
  | Y (s0, s1) ->
    let s0p, s0n = compl_decomp s0 in
    let s1p, s1n = compl_decomp s1 in
    let sp = cY s0p s1p in
    let sn = cY s0n s1n in
    let sc = cY (abs_compl s0) (abs_compl s1) in
    if card_prefixes sp + card_prefixes sn <= 1 + card_prefixes sc
	then (sp, sn)
	else (Top, sc)
  | P (p0, s0) ->
    let s0p, s0n = compl_decomp s0 in
    (cP p0 s0p, cP p0 s0n)

let rec dump chan = function
  | Bot -> output_string chan "∅"
  | Top -> output_string chan "*"
  | Y (s0, s1) ->
    output_char chan '(';
    dump chan s0;
    output_string chan ", ";
    dump chan s1;
    output_char chan ')'
  | P (p, s) -> output_string chan (Bitstring.to_string p); dump chan s
