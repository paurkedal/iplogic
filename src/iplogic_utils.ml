(* Copyright (C) 2012--2014  Petter Urkedal <paurkedal@gmail.com>
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

open Iplogic_address
open Iplogic_types
open Unprime_array
open Iplogic_diag

let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos)

let expr_loc = function
  | Expr_var (loc, _)
  | Expr_value (loc, _)
  | Expr_isecn (loc, _, _)
  | Expr_union (loc, _, _)
  | Expr_compl (loc, _, _)
  | Expr_range (loc, _, _)
  | Expr_cat (loc, _)
    -> loc

let cond_loc = function
  | Cond_const (loc, _)
  | Cond_and (loc, _, _)
  | Cond_or (loc, _, _)
  | Cond_not (loc, _)
  | Cond_flag (loc, _, _)
  | Cond_call (loc, _)
    -> loc

let chain_loc = function
  | Chain_if (loc, _, _, _)
  | Chain_continue loc
  | Chain_decision (loc, _)
  | Chain_return loc
  | Chain_fail loc
  | Chain_goto (loc, _)
  | Chain_call (loc, _, _)
  | Chain_log (loc, _, _)
    -> loc

let def_loc = function
  | Def_val (loc, _, _)
  | Def_val_type (loc, _, _)
  | Def_cond (loc, _, _)
  | Def_chain (loc, _, _, _)
    -> loc

let vtype_to_string = function
  | Vtype_int -> "int"
  | Vtype_string -> "string"
  | Vtype_ipaddrs -> "ipaddr"

let value_type = function
  | Value_int _ -> Vtype_int
  | Value_string _ -> Vtype_string
  | Value_ipaddrs _ | Value_dnsname _ -> Vtype_ipaddrs

let resolve loc name =
  try
    let h = Unix.gethostbyname name in
    let add_addr inaddr =
      let addr = ipaddrs_of_string (Unix.string_of_inet_addr inaddr) in
      Bitpath_cover.union addr in
    Array.fold add_addr h.Unix.h_addr_list Bitpath_cover.empty
  with Not_found ->
    failf ~loc "Failed to resolve %s." name
