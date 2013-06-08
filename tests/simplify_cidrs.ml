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

open Printf
open Iplogic_address

let simpl sP sN =
  let s = Bitpath_cover.rel_compl sN sP in
  let sN', sP' = Bitpath_cover.compl_decomp s in
  printf "%s ∖ %s\n" (ipaddrs_to_v6string sP) (ipaddrs_to_v6string sN);
  printf "%s ∖ %s\n" (ipaddrs_to_v6string sP') (ipaddrs_to_v6string sN')

let () =
  match Sys.argv with
  | [|_; strP; strN|] ->
    simpl (ipaddrs_of_string strP) (ipaddrs_of_string strN)
  | [|_; strP|] ->
    simpl (ipaddrs_of_string strP) Bitpath_cover.empty
  | _ ->
    eprintf "Wrong number of arguments.\n"; exit 64
