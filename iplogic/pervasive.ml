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

let ident x = x
let konst x y = x
let (|<) f g x = f (g x)

module Option = struct
    let iter f = function
      | None -> ()
      | Some x -> f x
end

module List = struct
    include List

    let rec fold f = function
      | [] -> ident
      | x :: xs -> fold f xs |< f x
end

module Char = struct
    include Char

    let is_space = function
	' ' | '\t' | '\n' | '\r' -> true
	| _ -> false

    let is_digit ch = '0' <= ch && ch <= '9'
    let is_xdigit ch = is_digit ch || 'a' <= ch && ch <= 'f'
				   || 'A' <= ch && ch <= 'F'
    let is_lower ch = 'a' <= ch && ch <= 'z'
    let is_upper ch = 'A' <= ch && ch <= 'Z'
    let is_alpha ch = is_lower ch || is_upper ch
    let is_alnum ch = is_digit ch || is_alpha ch
end

module String = struct
    include String

    let rec skip_while f s i =
	if i < length s && f s.[i] then skip_while f s (i + 1) else i

    let rec rskip_while f s i =
	if i > 0 && f s.[i - 1] then rskip_while f s (i - 1) else i

    let skip_until f s i = skip_while (not |< f) s i
    let rskip_until f s j = rskip_while (not |< f) s j

    let slice i j s = sub s i (j - i)
    let slice_to i s = sub s 0 i
    let slice_from i s = sub s i (length s - i)

    let split_at i s = (slice_to i s, slice_from i s)

    let split_on_first f s =
	let i = skip_until f s 0 in
	if i = length s then
	    invalid_arg "split_on_first: Missing separator." else
	(slice_to i s, slice_from (i + 1) s)

    let split_on_last f s =
	let j = rskip_until f s (length s) in
	if j = 0 then invalid_arg "split_on_last: Missing separator." else
	(slice_to (j - 1) s, slice_from j s)

    let strip s =
	let j = rskip_while Char.is_space s (length s) in
	if j = 0 then "" else slice (skip_while Char.is_space s 0) j s
end

module Int = struct

    type t = int

    let compare = compare

    let floor_log2 n =
	let rec loop j n l =
	    if j = 0 then (assert (n = 1); l) else
	    if n lsr j = 0 then loop (j / 2) n l else
	    loop (j / 2) (n lsr j) (l + j) in
	if n <= 0 then invalid_arg "floor_log2 on non-positive argument." else
	loop 32 n 0 (* supports up to 64 bits *)

    let bitcount16 n =
	let n = (n land 0x5555) + ((n lsr 1) land 0x5555) in
	let n = (n land 0x3333) + ((n lsr 2) land 0x3333) in
	let n = (n land 0x0f0f) + ((n lsr 4) land 0x0f0f) in
	let n = (n land 0x00ff) + ((n lsr 8) land 0x00ff) in
	n

    let rec bitcount n =
	if n = 0 then 0 else
	bitcount16 n + bitcount (n lsr 16)
end
