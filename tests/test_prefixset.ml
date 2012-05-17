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
open Pervasive

let (===) = Prefixset.equal

let random_bitstring n = Bitstring.init n (fun _ -> Random.bool ())

let random_prefixset max_width =
    if max_width = 0 then
	if Random.bool () then Prefixset.universe else Prefixset.empty else
    let width = Random.int max_width in
    let rec loop n_add s =
	if n_add = 0 then s else
	let p = random_bitstring width in
	loop (n_add - 1) (Prefixset.add p s) in
    let n_add = 1 lsl width in
    loop n_add Prefixset.empty

let verbose = ref false

let show_prefixset sv s =
    if not !verbose then () else
    if true then begin
	printf "%s = {" sv;
	let count = ref 0 in
	Prefixset.iter_prefixes (fun x ->
	    if !count > 0 then print_string ", ";
	    count := !count + 1;
	    if Bitstring.length x > 0 then print_string (Bitstring.to_string x);
	    print_char '*') s;
	printf "}\n"
    end else begin
	printf "%s = " sv; Prefixset.dump stdout s; output_char stdout '\n'
    end;
    flush stdout

let test max_width =
    let sA = random_prefixset max_width in
    let sB = random_prefixset max_width in
    show_prefixset "A" sA;
    show_prefixset "B" sB;
    assert (Prefixset.valid sA);
    assert (Prefixset.valid sB);

    assert (Prefixset.card_prefixes sA =
	    Prefixset.fold_prefixes (fun _ -> (+) 1) sA 0);

    let sAuB = Prefixset.union sA sB in
    let sAnB = Prefixset.isecn sA sB in
    show_prefixset "A ∪ B" sAuB;
    show_prefixset "A ∩ B" sAnB;
    assert (Prefixset.valid sAuB);
    assert (Prefixset.valid sAnB);
    assert (sAuB === Prefixset.union sB sA); (* A ∪ B = B ∪ A *)
    assert (sAnB === Prefixset.isecn sB sA); (* A ∩ B = B ∩ A *)

    let scA = Prefixset.abs_compl sA in
    show_prefixset "∁A" scA;

    assert (Prefixset.union sA scA === Prefixset.universe); (* A ∪ ∁A = U *)
    assert (Prefixset.isecn sA scA === Prefixset.empty);    (* A ∩ ∁A = ∅ *)

    let sAcB = Prefixset.compl sA sB in
    let sBcA = Prefixset.compl sB sA in
    show_prefixset "A ∖ B" sAcB;
    show_prefixset "B ∖ A" sBcA;
    assert (Prefixset.valid sAcB);
    assert (Prefixset.valid sBcA);

    let sBcA' = Prefixset.isecn scA sB in
    show_prefixset "∁A ∩ B" sBcA';
    assert (Prefixset.valid sBcA');
    assert (sBcA' === sBcA);
    assert (Prefixset.isecn sAcB sBcA === Prefixset.empty);

    let sA' = Prefixset.compl sAuB sBcA in
    let sB' = Prefixset.compl sAuB sAcB in
    show_prefixset "A ∪ B ∖ (B ∖ A)" sA';
    show_prefixset "A ∪ B ∖ (A ∖ B)" sB';
    assert (Prefixset.valid sA');
    assert (Prefixset.valid sB');
    assert (sB' === sB);
    assert (sA' === sA);
    assert (Prefixset.compl sAuB sAnB === Prefixset.union sAcB sBcA);

    let sP, sN = Prefixset.compl_decomp sA in
    show_prefixset "P" sP;
    show_prefixset "N" sN;
    let sPcN = Prefixset.compl sP sN in
    show_prefixset "P ∖ N" sPcN;
    assert (sPcN === sA);
    assert (Prefixset.card_prefixes sP + Prefixset.card_prefixes sN <=
	    Prefixset.card_prefixes sA)

let () =
    Arg.parse [("-v", Arg.Set verbose, "Dump computed values.")] (konst ()) "";
    for testnum = 0 to 999 do
	test (Random.int 12)
    done
