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

open Scanf
open Printf
open Pervasive

type ipaddr = Bitstring.t
type ipaddrs = Prefixset.t

let ipv4_prefix =
    Bitstring.of_list16 [0; 0; 0; 0; 0; 0xffff]

let ipaddr_is_v4 = Bitstring.has_prefix ipv4_prefix

let parse_ipv4_comps s =
    let check i =
	if i < 0 then
	    invalid_arg "IPv6 address octet cannot be negative.";
	if i > 255 then
	    invalid_arg "IPv4 address octet cannot be larger than 255.";
	in
    sscanf s "%d.%d.%d.%d" begin fun i0 i1 i2 i3 ->
	check i0; check i1; check i2; check i3;
	[i0 lsl 8 lor i1; i2 lsl 8 lor i3]
    end

let parse_ipv4 s =
    Bitstring.of_list16 (0::0::0::0::0::0xffff::parse_ipv4_comps s)

let parse_ipv6 s =
    let n = String.length s in
    let rec loop i parts comps =
	if i >= n then comps :: parts else
	let j = String.skip_while Char.is_xdigit s i in
	if j < n && s.[j] = '.' then
	    let v4comps = parse_ipv4_comps (String.slice i n s) in
	    List.rev_append v4comps comps :: parts else
	let comps' =
	    if i = j then comps else
	    sscanf (String.slice i j s) "%x" ident :: comps in
	if j + 1 < n && s.[j + 1] = ':'
	    then loop (j + 2) (comps' :: parts) []
	    else loop (j + 1) parts comps' in
    let comps =
	match loop 0 [] [] with
	  | [] -> invalid_arg "Cannot parse empty string an IP address."
	  | [comps] ->
	    if List.length comps <> 8 then
		invalid_arg "Wrong number of hexadecatets in IPv6 address." else
	    List.rev comps
	  | [comps0; comps1] ->
	    let n = 8 - List.length comps0 - List.length comps1 in
	    if n < 0 then
		invalid_arg "Too many hexadecatets in IPv6 address." else
	    let rec pad n comps =
		if n = 0 then comps else pad (n - 1) (0 :: comps) in
	    List.rev_append comps0 (pad n (List.rev comps1))
	  | _ ->
	    invalid_arg "IPv6 address cannot contain multiple ::-separators." in
    Bitstring.of_list16 comps

let ipaddr_of_string s =
    if String.contains s ':' then parse_ipv6 s
			     else parse_ipv4 s

let ipaddr_to_v4string addr =
    if not (ipaddr_is_v4 addr) then
	invalid_arg "Not an IPv4 address." else
    let buf = Buffer.create 15 in
    bprintf buf "%d.%d.%d.%d"
	(Bitstring.get8 0 addr) (Bitstring.get8 1 addr)
	(Bitstring.get8 2 addr) (Bitstring.get8 3 addr);
    Buffer.contents buf

let ipaddr_to_v6string addr =
    let buf = Buffer.create 39 in
    (* TODO: Minimise addresses. *)
    bprintf buf "%x:%x:%x:%x:%x:%x:%x:%x"
	(Bitstring.get16 0 addr) (Bitstring.get16 1 addr)
	(Bitstring.get16 2 addr) (Bitstring.get16 3 addr)
	(Bitstring.get16 4 addr) (Bitstring.get16 5 addr)
	(Bitstring.get16 8 addr) (Bitstring.get16 7 addr);
    Buffer.contents buf

let parse_cidr cidr =
    if not (String.contains cidr '/') then
	Prefixset.of_prefix (ipaddr_of_string cidr) else
    let addr_s, n_s = String.split_on_last ((=) '/') cidr in
    try
	let n = int_of_string n_s in
	let addr = ipaddr_of_string addr_s in
	Prefixset.of_prefix (Bitstring.prefix n addr)
    with Failure _ -> invalid_arg (sprintf "Invalid CIDR adderss \"%s\"." cidr)

let ipaddrs_of_string cidrs =
    let n = String.length cidrs in
    let rec loop i addrs =
	if i >= n then addrs else
	let j = String.skip_until ((=) ',') cidrs i in
	if i = j then invalid_arg "Empty element in address list." else
	loop (j + 1)
	     (Prefixset.union (parse_cidr (String.slice i j cidrs)) addrs) in
    loop 0 Prefixset.empty

let ipaddrs_to_v4string c =
    let buf = Buffer.create 64 in
    let add_net addr =
	let a = Array.make 4 0 in
	Bitstring.iteri8 (fun i x -> Array.set a i x) addr;
	if Buffer.length buf > 0 then Buffer.add_char buf ',';
	bprintf buf "%d.%d.%d.%d" a.(0) a.(1) a.(2) a.(3);
	if Bitstring.length addr < 32 then
	    bprintf buf "/%d" (Bitstring.length addr) in
    Prefixset.iter_prefixes add_net (Prefixset.follow ipv4_prefix c);
    Buffer.contents buf

let ipaddrs_to_v6string c =
    let buf = Buffer.create 64 in
    let add_net addr =
	let a = Array.make 8 0 in
	Bitstring.iteri16 (fun i x -> Array.set a i x) addr;
	if Buffer.length buf > 0 then Buffer.add_char buf ',';
	(* TODO: Minimise addresses. *)
	bprintf buf "%x:%x:%x:%x:%x:%x:%x:%x"
	    a.(0) a.(1) a.(2) a.(3) a.(4) a.(5) a.(6) a.(7);
	if Bitstring.length addr < 128 then
	    bprintf buf "/%d" (Bitstring.length addr) in
    Prefixset.iter_prefixes add_net c;
    Buffer.contents buf

let ipaddrs_to_string ?(v6 = false) c =
    if v6 then ipaddrs_to_v6string c else ipaddrs_to_v4string c
