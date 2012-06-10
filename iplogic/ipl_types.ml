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

type loc = Lexing.position

type vtype =
   | Vtype_int
   | Vtype_string
   | Vtype_ipaddrs

type value =
   | Value_int of int
   | Value_string of string
   | Value_ipaddrs of Ipaddr.ipaddrs
   | Value_dnsname of string

type expr =
   | Expr_var of loc * string
   | Expr_value of loc * value
   | Expr_isecn of loc * expr * expr
   | Expr_union of loc * expr * expr
   | Expr_compl of loc * expr * expr
   | Expr_range of loc * value * value

type cond =
   | Cond_const of loc * bool
   | Cond_and of loc * cond * cond
   | Cond_or of loc * cond * cond
   | Cond_not of loc * cond
   | Cond_flag of loc * string * expr
   | Cond_call of loc * string

type decision =
   | Accept
   | Reject
   | Drop

type chain =
   | Chain_if of loc * cond * chain * chain
   | Chain_decision of loc * decision
   | Chain_return of loc
   | Chain_fail of loc
   | Chain_goto of loc * string
   | Chain_call of loc * string * chain
   | Chain_log of loc * (string * expr) list * chain

type def =
   | Def_val of loc * string * expr
   | Def_val_type of loc * string * string
   | Def_cond of loc * string * cond
   | Def_chain of loc * string * chain
