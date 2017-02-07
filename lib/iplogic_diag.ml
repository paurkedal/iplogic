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

open Printf
open Unprime_option

let bprint_loc buf (lb, ub) =
  if lb.Lexing.pos_cnum = ub.Lexing.pos_cnum then
    bprintf buf "%s:%d,%d: "
            lb.Lexing.pos_fname
            lb.Lexing.pos_lnum (lb.Lexing.pos_cnum - lb.Lexing.pos_bol)
  else if lb.Lexing.pos_lnum = ub.Lexing.pos_lnum then
    bprintf buf "%s:%d,%d-%d: "
            lb.Lexing.pos_fname lb.Lexing.pos_lnum
            (lb.Lexing.pos_cnum - lb.Lexing.pos_bol)
            (ub.Lexing.pos_cnum - ub.Lexing.pos_bol)
  else
    bprintf buf "%s:%d,%d-%d,%d: "
            lb.Lexing.pos_fname
            lb.Lexing.pos_lnum (lb.Lexing.pos_cnum - lb.Lexing.pos_bol)
            ub.Lexing.pos_lnum (ub.Lexing.pos_cnum - ub.Lexing.pos_bol)

let sprint_loc loc =
  let buf = Buffer.create 10 in
  bprint_loc buf loc; Buffer.contents buf

let eprint_loc loc = output_string stderr (sprint_loc loc)

let errf ?loc msg =
  ksprintf (fun s -> Option.iter eprint_loc loc; eprintf "%s\n" s) msg

let failf ?loc msg =
  let on_msg s =
    let buf = Buffer.create 10 in
    Option.iter (bprint_loc buf) loc; bprintf buf "%s\n" s;
    raise (Failure (Buffer.contents buf)) in
  ksprintf on_msg msg
