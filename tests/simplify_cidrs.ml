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

open Printf
open Iplogic

let simpl sP sN =
    let s = Prefixset.compl sP sN in
    let sP', sN' = Prefixset.compl_decomp s in
    printf "%s ∖ %s\n"
	(Ipaddr.ipaddrs_to_v6string sP') (Ipaddr.ipaddrs_to_v6string sN')

let () =
    match Sys.argv with
      | [|_; strP; strN|] ->
	simpl (Ipaddr.ipaddrs_of_string strP) (Ipaddr.ipaddrs_of_string strN)
      | [|_; strP|] ->
	simpl (Ipaddr.ipaddrs_of_string strP) Prefixset.empty
      | _ ->
	eprintf "Wrong number of arguments.\n"; exit 64
