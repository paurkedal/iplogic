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

module Option : sig
    val iter : ('a -> unit) -> 'a option -> unit
end

module List : sig
    include module type of List

    val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
end

module Char : sig
    include module type of Char

    val is_space : char -> bool
    val is_digit : char -> bool
    val is_lower : char -> bool
    val is_upper : char -> bool
    val is_alpha : char -> bool
    val is_alnum : char -> bool
end

module String : sig
    include module type of String

    val skip_while : (char -> bool) -> string -> int -> int
    val rskip_while : (char -> bool) -> string -> int -> int
    val skip_until : (char -> bool) -> string -> int -> int
    val rskip_until : (char -> bool) -> string -> int -> int
    val slice : int -> int -> string -> string
    val slice_to : int -> string -> string
    val slice_from : int -> string -> string
    val split_at : int -> string -> string * string
    val split_on_first : (char -> bool) -> string -> string * string
    val split_on_last : (char -> bool) -> string -> string * string
end

module Int : sig
    type t = int

    val compare : int -> int -> int

    val bitcount : int -> int
end
