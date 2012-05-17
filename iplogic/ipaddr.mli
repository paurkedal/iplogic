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

type ipaddr = Bitstring.t

val ipaddr_of_string : string -> ipaddr

val ipaddr_is_v4 : ipaddr -> bool
val ipaddr_to_v4string : ipaddr -> string
val ipaddr_to_v6string : ipaddr -> string

type ipaddrs = Prefixset.t

val ipaddrs_of_string : string -> ipaddrs
val ipaddrs_to_v4string : ipaddrs -> string
val ipaddrs_to_v6string : ipaddrs -> string
val ipaddrs_to_string : ?v6 : bool -> ipaddrs -> string
