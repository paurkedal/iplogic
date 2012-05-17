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

val length : t -> int
val length8 : t -> int
val length16 : t -> int

val get : int -> t -> bool
val get8 : int -> t -> int
val get16 : int -> t -> int

val empty : t
val singleton : bool -> t

val init : int -> (int -> bool) -> t
val init8 : int -> (int -> int) -> t
val init16 : int -> (int -> int) -> t

val of_list : bool list -> t
val of_list8 : int list -> t
val of_list16 : int list -> t

val equal : t -> t -> bool

val foldi : (int -> bool -> 'a -> 'a) -> t -> 'a -> 'a
val foldi8 : (int -> int -> 'a -> 'a) -> t -> 'a -> 'a
val foldi16 : (int -> int -> 'a -> 'a) -> t -> 'a -> 'a

val iteri : (int -> bool -> unit) -> t -> unit
val iteri8 : (int -> int -> unit) -> t -> unit
val iteri16 : (int -> int -> unit) -> t -> unit

val cat : t -> t -> t

val has_prefix : t -> t -> bool

val slice : int -> int -> t -> t
val prefix : int -> t -> t
val common_prefix_length : t -> t -> int
val common_prefix : t -> t -> t
val common_prefix_length_from : int -> t -> int -> t -> int

val to_string : t -> string
val of_string : string -> t