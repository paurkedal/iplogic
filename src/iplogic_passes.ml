(* Copyright (C) 2012--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

open Iplogic_diag
open Iplogic_types
open Iplogic_utils
open Unprime
open Unprime_list
module String_map = Map.Make (String)

module Env =
  struct
    type t = {
        emap : (vtype * expr) String_map.t;
        cmap : cond String_map.t;
        chmap : (policy * chain) String_map.t String_map.t;
    }
    let empty = {
        emap = String_map.empty;
        cmap = String_map.empty;
        chmap = String_map.empty;
    }
    let define_expr loc en e env =
      {env with emap = String_map.add en e env.emap}
    let define_cond loc cn c env =
      {env with cmap = String_map.add cn c env.cmap}
    let define_chain loc tn chn policy ch env =
      let chmap' =
        let tm = try String_map.find tn env.chmap
                 with Not_found -> String_map.empty in
        String_map.add tn (String_map.add chn (policy, ch) tm) env.chmap in
      {env with chmap = chmap'}

    let lookup_expr loc en env =
        try String_map.find en env.emap with
        | Not_found -> failf ~loc "Value %s is not defined." en
    let lookup_cond loc cn env =
        try String_map.find cn env.cmap with
        | Not_found -> failf ~loc "Condition %s is not defined." cn
  end

let rec check_expr env = function
  | Expr_var (loc, en) -> fst (Env.lookup_expr loc en env)
  | Expr_value (loc, v) -> value_type v
  | Expr_isecn (loc, e0, e1) | Expr_union (loc, e0, e1)
  | Expr_compl (loc, e0, e1) ->
    let e0t = check_expr env e0 in
    let e1t = check_expr env e1 in
    if e0t <> e1t then
        failf ~loc "Incompatible types %s and %s in set expression."
            (vtype_to_string e0t) (vtype_to_string e1t);
    e0t
  | Expr_range (loc, e0, e1) ->
    begin match value_type e0, value_type e1 with
    | Vtype_int, Vtype_int -> Vtype_int
    | _ -> failf ~loc "Expected integers in range expression."
    end
  | Expr_cat (loc, es) -> Vtype_string

let rec denote_ipaddrs = function
  | Expr_isecn (_, e0, e1) ->
    Bitpath_cover.isecn (denote_ipaddrs e0) (denote_ipaddrs e1)
  | Expr_union (_, e0, e1) ->
    Bitpath_cover.union (denote_ipaddrs e0) (denote_ipaddrs e1)
  | Expr_compl (_, e0, e1) ->
    Bitpath_cover.rel_compl (denote_ipaddrs e1) (denote_ipaddrs e0)
  | Expr_value (_, Value_ipaddrs addrs) -> addrs
  | Expr_value (loc, Value_dnsname dnsname) -> resolve loc dnsname
  | Expr_value _ | Expr_range _ | Expr_var _ | Expr_cat _ -> assert false

let rec simplify_expr env = function
  | Vtype_int -> ident
  | Vtype_string ->
    begin function
    | Expr_cat (loc, es) ->
      Expr_cat (loc, List.map (check_and_simplify_expr env) es)
    | Expr_value (loc, v) as e -> e
    | _ -> assert false
    end
  | Vtype_ipaddrs ->
    fun e -> Expr_value (expr_loc e, Value_ipaddrs (denote_ipaddrs e))
and check_and_simplify_expr env e = simplify_expr env (check_expr env e) e

let rec pass1_expr env = function
  | Expr_var (loc, en) -> snd (Env.lookup_expr loc en env)
  | Expr_value _ | Expr_range _ as e -> e
  | Expr_isecn (loc, e0, e1) ->
    Expr_isecn (loc, pass1_expr env e0, pass1_expr env e1)
  | Expr_union (loc, e0, e1) ->
    Expr_union (loc, pass1_expr env e0, pass1_expr env e1)
  | Expr_compl (loc, e0, e1) ->
    Expr_compl (loc, pass1_expr env e0, pass1_expr env e1)
  | Expr_cat (loc, es) ->
    Expr_cat (loc, List.map (pass1_expr env) es)

let pass1s_expr env e =
  let et = check_expr env e in
  simplify_expr env et (pass1_expr env e)

let pass1s_options env opts =
  List.map (fun (opt, arg) -> (opt, pass1s_expr env arg)) opts

let rec pass1_cond env = function
  | Cond_const _ as c -> c
  | Cond_and (loc, c0, c1) ->
    Cond_and (loc, pass1_cond env c0, pass1_cond env c1)
  | Cond_or (loc, c0, c1) ->
    Cond_or (loc, pass1_cond env c0, pass1_cond env c1)
  | Cond_not (loc, c) ->
    Cond_not (loc, pass1_cond env c)
  | Cond_flag (loc, flag, e) ->
    Cond_flag (loc, flag, pass1s_expr env e)
  | Cond_call (loc, cn) ->
    Env.lookup_cond loc cn env

let rec pass1_chain = function
  | Chain_if (loc, c, cq, ccq) -> fun env ->
    let c' = pass1_cond env c in
    let cq', env' = pass1_chain cq env in
    let ccq', env'' = pass1_chain ccq env' in
    Chain_if (loc, c', cq', ccq'), env''
  | Chain_continue loc -> fun env ->
    Chain_continue loc, env
  | Chain_decision (loc, Alter (tgt, opts)) -> fun env ->
    Chain_decision (loc, Alter (tgt, pass1s_options env opts)), env
  | Chain_decision _ | Chain_return _ | Chain_fail _ | Chain_goto _ as r ->
    fun env -> r, env
  | Chain_call (loc, chn, ch) -> fun env ->
    let ch', env' = pass1_chain ch env in
    Chain_call (loc, chn, ch'), env'
  | Chain_log (loc, opts, ch) -> fun env ->
    let ch', env' = pass1_chain ch env in
    Chain_log (loc, pass1s_options env opts, ch'), env'

let pass1_def = function
  | Def_val (loc, en, e) -> fun env ->
    let et = check_expr env e in
    let e' = (simplify_expr env et (pass1s_expr env e)) in
    Env.define_expr loc en (et, e') env
  | Def_val_type _ -> failwith "Value declarations are not implemented."
  | Def_cond (loc, cn, c) -> fun env ->
    Env.define_cond loc cn (pass1_cond env c) env
  | Def_chain (loc, tn, chn, policy, ch) -> fun env ->
    let ch', env' = pass1_chain ch env in
    Env.define_chain loc tn chn policy ch' env'

let compile defs =
  let env = List.fold pass1_def defs Env.empty in
  List.rev
    (String_map.fold
      (fun tn tm ->
        String_map.fold (fun chn chd acc -> (tn, chn, chd) :: acc) tm)
      env.Env.chmap
      [])
