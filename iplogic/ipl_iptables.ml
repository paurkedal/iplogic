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

open Ipl_types
open Shell_monoid

let (>>) x y = SL[x; y]

let decision_to_string = function
  | Accept -> "ACCEPT"
  | Reject -> "REJECT"
  | Drop -> "DROP"

let value_to_string ?(v6 = false) ?(quote = false) = function
  | Value_int i -> string_of_int i
  | Value_string s -> if quote then "\"" ^ (String.escaped s) ^ "\"" else s
  | Value_ipaddrs addrs -> Ipaddr.ipaddrs_to_string ~v6 addrs
  | Value_dnsname s -> s

let rec expr_to_string ?(quote = false) = function
  | Expr_var _ -> invalid_arg "Variables should have been expanded."
  | Expr_value (loc, v) -> value_to_string ~quote v
  | Expr_union (loc, x, y) ->
    expr_to_string ~quote x ^ "," ^ expr_to_string ~quote y
  | Expr_range (loc, x, y) ->
    value_to_string ~quote x ^ ":" ^ value_to_string ~quote y
  | Expr_isecn _ -> invalid_arg "Intersections should have been eliminated."
  | Expr_compl _ -> invalid_arg "Complements should have been eliminated."


let rec emit_cond = function
  | Cond_const (_, true) -> AL []
  | Cond_const (_, false) ->
    invalid_arg "False conditions should have been eliminated."
  | Cond_and (_, c0, c1) -> AL[emit_cond c0; emit_cond c1]
  | Cond_or _ -> invalid_arg "Disjunction should have been eliminated."
  | Cond_not (loc, c) ->
    (match c with Cond_flag _ -> ()
		| _ -> invalid_arg "Negation should have been distributed.");
    AL [AV"!"; emit_cond c]
  | Cond_flag (_, flag, expr) -> AL[AV flag; AQ (expr_to_string expr)]
  | Cond_call _ -> invalid_arg "Calls should have been inlined."

let rec emit_logopts = function
  | ("level", expr) :: opts ->
    (* check_type Vtype_int expr; *)
    AV"--log-level" :: AQ (expr_to_string expr) :: emit_logopts opts
  | ("prefix", expr) :: opts ->
    (* check_type Vtype_string expr; *)
    AV"--log-prefix" :: AQ (expr_to_string expr) :: emit_logopts opts
  | _ -> invalid_arg "Unhandled log option."

let emit_iptables qcn args =
    SC (AL [
	AV"iptables";
	AV"-t"; AQ (fst qcn);
	AV"-A"; AQ (snd qcn);
	args
    ])

let rec emit_chain' qcn = function
  | Chain_if (loc, Cond_const (_, true), cq, ccq) -> fun cond ->
    emit_chain' qcn cq cond
  | Chain_if (loc, Cond_const (_, false), cq, ccq) -> fun cond ->
    emit_chain' qcn ccq cond
  | Chain_if (loc, cond', cq, ccq) -> fun cond ->
    emit_chain' qcn cq (Cond_and (loc, cond, cond')) >>
    emit_chain' qcn ccq cond
  | Chain_decision (_, dec) -> fun cond ->
    emit_iptables qcn (AL[AV"-j"; AQ (decision_to_string dec);
				emit_cond cond])
  | Chain_return loc -> fun cond ->
    emit_iptables qcn (AL[AV"-j"; AV "RETURN"; emit_cond cond])
  | Chain_fail loc -> fun _ -> failwith "The fail keyword is not implemented."
  | Chain_goto (loc, cn) -> fun cond ->
    emit_iptables qcn (AL[AV"-g"; AQ cn; emit_cond cond])
  | Chain_call (loc, cn, cont) -> fun cond ->
    emit_iptables qcn (AL[AV"-j"; AQ cn; emit_cond cond]) >>
    emit_chain' qcn cont cond
  | Chain_log (loc, opts, cont) -> fun cond ->
    emit_iptables qcn
	(AL[AV"-j"; AV"LOG"; AL (emit_logopts opts); emit_cond cond]) >>
    emit_chain' qcn cont cond

let emit_chain qcn chain =
    emit_chain' qcn chain (Cond_const (Lexing.dummy_pos, true))
