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

type t

val equal : t -> t -> bool

val empty : t		val is_empty : t -> bool
val universe : t	val is_universe : t -> bool

val of_prefix : Bitstring.t -> t
val is_prefix : t -> bool
val to_prefix : t -> Bitstring.t

val add : Bitstring.t -> t -> t
val remove : Bitstring.t -> t -> t
val modify : Bitstring.t -> (t -> t) -> t -> t
val follow : Bitstring.t -> t -> t

val fold_prefixes : (Bitstring.t -> 'a -> 'a) -> t -> 'a -> 'a
val iter_prefixes : (Bitstring.t -> unit) -> t -> unit
val card_prefixes : t -> int

val isecn : t -> t -> t
val union : t -> t -> t
val compl : t -> t -> t
val abs_compl : t -> t

val valid : t -> bool
val dump : out_channel -> t -> unit

val compl_decomp : t -> t * t
