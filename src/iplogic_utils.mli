(* Copyright (C) 2012--2013  Petter Urkedal <paurkedal@gmail.com>
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

open Iplogic_types

val expr_loc : expr -> loc
val cond_loc : cond -> loc
val chain_loc : chain -> loc
val def_loc : def -> loc

val value_type : value -> vtype
val vtype_to_string : vtype -> string

val resolve : loc -> string -> Bitpath_cover.t
