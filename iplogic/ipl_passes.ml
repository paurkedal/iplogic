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

open Pervasive
open Ipl_types
module String_map = Map.Make (String)

module Env =
  struct
    type t = {
	emap : expr String_map.t;
	cmap : cond String_map.t;
	chmap : chain String_map.t;
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
    let define_chain loc chn ch env =
	{env with chmap = String_map.add chn ch env.chmap}

    let lookup_expr loc en env =
	try String_map.find en env.emap with
	| Not_found -> Diag.failf ~loc "Value %s is not defined." en
    let lookup_cond loc cn env =
	try String_map.find cn env.cmap with
	| Not_found -> Diag.failf ~loc "Condition %s is not defined." cn
  end

let rec pass1_expr env = function
  | Expr_var (loc, en) -> Env.lookup_expr loc en env
  | Expr_value _ | Expr_range _ as e -> e
  | Expr_isecn (loc, e0, e1) ->
    Expr_isecn (loc, pass1_expr env e0, pass1_expr env e1)
  | Expr_union (loc, e0, e1) ->
    Expr_union (loc, pass1_expr env e0, pass1_expr env e1)
  | Expr_compl (loc, e0, e1) ->
    Expr_compl (loc, pass1_expr env e0, pass1_expr env e1)

let rec pass1_cond env = function
  | Cond_true _ as c -> c
  | Cond_and (loc, c0, c1) ->
    Cond_and (loc, pass1_cond env c0, pass1_cond env c1)
  | Cond_or (loc, c0, c1) ->
    Cond_or (loc, pass1_cond env c0, pass1_cond env c1)
  | Cond_not (loc, c) ->
    Cond_not (loc, pass1_cond env c)
  | Cond_flag (loc, flag, e) ->
    Cond_flag (loc, flag, pass1_expr env e)
  | Cond_call (loc, cn) ->
    Env.lookup_cond loc cn env

let rec pass1_chain = function
  | Chain_if (loc, c, cq, ccq) -> fun env ->
    let c' = pass1_cond env c in
    let cq', env' = pass1_chain cq env in
    let ccq', env'' = pass1_chain ccq env' in
    Chain_if (loc, c', cq', ccq'), env''
  | Chain_decision _ | Chain_return _ | Chain_fail _ | Chain_goto _ as r ->
    fun env -> r, env
  | Chain_call (loc, chn, ch) -> fun env ->
    let ch', env' = pass1_chain ch env in
    Chain_call (loc, chn, ch'), env'
  | Chain_log (loc, opts, ch) -> fun env ->
    let opts' = List.map (fun (opt, arg) -> (opt, pass1_expr env arg)) opts in
    let ch', env' = pass1_chain ch env in
    Chain_log (loc, opts', ch'), env'

let pass1_def = function
  | Def_val (loc, en, e) -> fun env ->
    Env.define_expr loc en (pass1_expr env e) env
  | Def_val_type _ -> failwith "Value declarations are not implemented."
  | Def_cond (loc, cn, c) -> fun env ->
    Env.define_cond loc cn (pass1_cond env c) env
  | Def_chain (loc, chn, ch) -> fun env ->
    let ch', env' = pass1_chain ch env in
    Env.define_chain loc chn ch' env'

let compile defs =
    let env = List.fold pass1_def defs Env.empty in
    String_map.bindings env.Env.chmap
