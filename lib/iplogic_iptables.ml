(* Copyright (C) 2012--2017  Petter A. Urkedal <paurkedal@gmail.com>
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
open Iplogic_shell
open Printf
open Unprime_list

let (>>) x y = SL[x; y]

let iptables_command = ref "iptables"

let set_of_list xs =
  Iplogic_utils.(List.fold String_set.add xs String_set.empty)

let builtin_filter_targets =
  set_of_list ["INPUT"; "FORWARD"; "OUTPUT"]
let builtin_nat_targets =
  set_of_list ["PREROUTING"; "INPUT"; "OUTPUT"; "POSTROUTING"]
let builtin_mangle_targets =
  set_of_list ["PREROUTING"; "INPUT"; "FORWARD"; "OUTPUT"; "POSTROUTING"]
let builtin_raw_targets =
  set_of_list ["PREROUTING"; "OUTPUT"]
let builtin_security_targets =
  set_of_list ["INPUT"; "FORWARD"; "OUTPUT"]
let builtin_targets_for = function
  | "filter" -> builtin_filter_targets
  | "nat" -> builtin_nat_targets
  | "mangle" -> builtin_mangle_targets
  | "raw" -> builtin_raw_targets
  | "security" -> builtin_security_targets
  | tn -> ksprintf invalid_arg "Invalid table %s." tn

let value_to_string ?(v6 = false) ?(quote = false) = function
  | Value_int i -> string_of_int i
  | Value_string s -> if quote then "\"" ^ (String.escaped s) ^ "\"" else s
  | Value_ipaddrs addrs -> ipaddrs_to_string ~v6 addrs
  | Value_dnsname s -> s

let rec expr_to_string ?(quote = false) = function
  | Expr_var _ -> invalid_arg "Variables should have been expanded."
  | Expr_value (_, v) -> value_to_string ~quote v
  | Expr_union (_, x, y) ->
    expr_to_string ~quote x ^ "," ^ expr_to_string ~quote y
  | Expr_range (_, x, y) ->
    value_to_string ~quote x ^ ":" ^ value_to_string ~quote y
  | Expr_isecn _ -> invalid_arg "Intersections should have been eliminated."
  | Expr_compl _ -> invalid_arg "Complements should have been eliminated."
  | Expr_cat (_, es) -> String.concat "" (List.map expr_to_string es)

let rec emit_cond = function
  | Cond_const (_, true) -> AL []
  | Cond_const (_, false) ->
    invalid_arg "False conditions should have been eliminated."
  | Cond_and (_, c0, c1) -> AL[emit_cond c0; emit_cond c1]
  | Cond_or _ -> invalid_arg "Disjunction should have been eliminated."
  | Cond_not (_, c) ->
    (match c with Cond_flag _ -> ()
                | _ -> invalid_arg "Negation should have been distributed.");
    AL [AV"!"; emit_cond c]
  | Cond_flag (_, flag, expr) -> AL[AV flag; AQ (expr_to_string expr)]
  | Cond_call _ -> invalid_arg "Calls should have been inlined."

let rec emit_options = function
  | [] -> []
  | (fl, v) :: opts -> AV fl :: AQ (expr_to_string v) :: emit_options opts

let rec emit_logopts = function
  | [] -> []
  | ("--log-level", expr) :: opts ->
    (* check_type Vtype_int expr; *)
    AV"--log-level" :: AQ (expr_to_string expr) :: emit_logopts opts
  | ("--log-prefix", expr) :: opts ->
    (* check_type Vtype_string expr; *)
    AV"--log-prefix" :: AQ (expr_to_string expr) :: emit_logopts opts
  | (opt, _) :: _ -> ksprintf invalid_arg "Unhandled log option %s." opt

let emit_iptables op qcn args =
  SC (AL [
    AV !iptables_command;
    AV"-t"; AQ (fst qcn);
    AV op; AQ (snd qcn);
    args
  ])

let emit_iptables_A qcn args = emit_iptables "-A" qcn args
let emit_iptables_N qcn = SLor [emit_iptables "-N" qcn (AV "2>/dev/null"); SL[]]

let emit_chainpolicy qcn policy =
  let setpol policyname =
    SC (AL [
      AV !iptables_command;
      AV"-t"; AQ (fst qcn);
      AV"-P"; AQ (snd qcn);
      AQ policyname;
    ]) in
  match policy with
  | Policy_none -> SL []
  | Policy_accept -> setpol "ACCEPT"
  | Policy_drop ->   setpol "DROP"

let rec emit_chain' qcn = function
  | Chain_if (_, Cond_const (_, true), cq, _) -> fun cond ->
    emit_chain' qcn cq cond
  | Chain_if (_, Cond_const (_, false), _, ccq) -> fun cond ->
    emit_chain' qcn ccq cond
  | Chain_if (loc, cond', cq, ccq) -> fun cond ->
    emit_chain' qcn cq (Cond_and (loc, cond, cond')) >>
    emit_chain' qcn ccq cond
  | Chain_continue _ -> fun _ -> SL []
  | Chain_decision (_, Accept) -> fun cond ->
    emit_iptables_A qcn (AL[AV"-j"; AV "ACCEPT"; emit_cond cond])
  | Chain_decision (_, Alter (t, opts)) -> fun cond ->
    emit_iptables_A qcn (AL[emit_cond cond; AV"-j";AV t; AL(emit_options opts)])
  | Chain_decision (_, Reject) -> fun cond ->
    emit_iptables_A qcn (AL[AV"-j"; AV "REJECT"; emit_cond cond])
  | Chain_decision (_, Drop) -> fun cond ->
    emit_iptables_A qcn (AL[AV"-j"; AV "DROP"; emit_cond cond])
  | Chain_return _ -> fun cond ->
    emit_iptables_A qcn (AL[AV"-j"; AV "RETURN"; emit_cond cond])
  | Chain_fail _ -> fun _ -> failwith "The fail keyword is not implemented."
  | Chain_goto (_, cn) -> fun cond ->
    emit_iptables_A qcn (AL[AV"-g"; AQ cn; emit_cond cond])
  | Chain_call (_, cn, cont) -> fun cond ->
    emit_iptables_A qcn (AL[AV"-j"; AQ cn; emit_cond cond]) >>
    emit_chain' qcn cont cond
  | Chain_log (_, opts, cont) -> fun cond ->
    emit_iptables_A qcn
        (AL[AV"-j"; AV"LOG"; AL (emit_logopts opts); emit_cond cond]) >>
    emit_chain' qcn cont cond

let emit_chain ?(emit_new = false) ?(emit_new_deps = false)
               ?(emit_flush = false) qcn (policy, chain) =
  let builtin_targets = builtin_targets_for (fst qcn) in
    (* Only needed when emit_new_deps, but called in any case to check that
     * the table name is valid. *)
  begin if emit_new_deps then
    let open Iplogic_utils in
    String_set.fold (fun cn m ->
                      if String_set.mem cn builtin_targets then m else
                      m >> emit_iptables_N (fst qcn, cn))
                    (chain_targets chain) (SL [])
  else SL [] end >>
  (if emit_new then emit_iptables_N qcn else SL []) >>
  emit_chainpolicy qcn policy >>
  (if emit_flush then emit_iptables "-F" qcn (AL []) else SL []) >>
  emit_chain' qcn chain (Cond_const (Iplogic_utils.dummy_loc, true))
