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

open Bigarray
open Pervasive

(* The bitstring is represented by a pair [(a, n)], where [n] is the number of
 * bits, and a is an array of hexadecatets making up the string.  Bit number
 * [i] of the bitstring is storet in bit number [i mod 16] of [a.(i / 16]). *)
type t = (int, int16_unsigned_elt, c_layout) Array1.t * int

let empty = (Array1.create int16_unsigned c_layout 0, 0)

let length (_, n) = n
let length8 (_, n) = (n + 7) / 8
let length16 (_, n) = (n + 15) / 16

let get i (a, _) = (Array1.get a (i / 16) lsr (i mod 16)) land 1 <> 0
let get8 i (a, _) = (Array1.get a (i / 2) lsr ((i mod 2) * 8)) land 255
let get16 i (a, _) = Array1.get a i

let init16 n f =
    let a = Array1.create int16_unsigned c_layout n in
    for i = 0 to n - 1 do Array1.set a i (f i) done;
    (a, n * 16)

let init8 n f =
    let a = Array1.create int16_unsigned c_layout ((n + 1) / 2) in
    for i = 0 to n / 2 - 1 do Array1.set a i (f (2*i) + 256 * f (2*i + 1)) done;
    if n mod 2 > 0 then Array1.set a (n / 2) (f (n - 1));
    (a, n * 8)

let init n f =
    let m = (n + 15) / 16 in
    let a = Array1.create int16_unsigned c_layout m in
    let ilast = n / 16 in
    let nlast = n mod 16 in
    let rec mk16 jleft j h =
        if jleft = 0 then h else
        mk16 (jleft - 1) (j - 1) (h * 2 + (if f j then 1 else 0)) in
    for i = 0 to ilast - 1 do
	Array1.set a i (mk16 16 (i * 16 + 15) 0) done;
    if nlast <> 0 then Array1.set a ilast (mk16 nlast (n - 1) 0);
    (a, n)

let equal (aL, nL) (aR, nR) =
    if nL <> nR then false else
    let m = (nL + 15) / 16 in
    let rec eq_from i =
	if i >= m then true else
	Array1.get aL i = Array1.get aR i && eq_from (i + 1) in
    eq_from 0

let foldi f (a, n) accu =
    let accu_x = ref accu in
    for i = 0 to n / 16 - 1 do
	let x = Array1.get a i in
	for j = 0 to 15 do
	    accu_x := f (16*i + j) ((x lsr j) land 1 = 1) !accu_x
	done
    done;
    let m_rem = n mod 16 in
    if m_rem > 0 then begin
	let i = n / 16 in
	let x = Array1.get a (n / 16) in
	for j = 0 to m_rem - 1 do
	    accu_x := f (16*i + j) ((x lsr j) land 1 = 1) !accu_x
	done
    end;
    !accu_x

let foldi8 f (a, n) accu =
    let accu_x = ref accu in
    let m8 = (n + 7) / 8 in
    for i = 0 to m8 / 2 - 1 do
	let x = Array1.get a i in
	accu_x := f (2*i) (x land 255) !accu_x;
	accu_x := f (2*i + 1) (x lsr 8) !accu_x
    done;
    if m8 mod 2 > 0 then begin
	let i = m8 / 2 in
	accu_x := f (2*i) (Array1.get a i) !accu_x
    end;
    !accu_x

let foldi16 f (a, n) accu =
    let accu_x = ref accu in
    for i = 0 to (n + 15) / 16 - 1 do
	accu_x := f i (Array1.get a i) !accu_x
    done;
    !accu_x

let cat (a0, n0) (a1, n1) =
    if n1 = 0 then (a0, n0) else
    if n0 = 0 then (a1, n1) else
    let m0, m1 = (n0 + 15) / 16, (n1 + 15) / 16 in
    let n = n0 + n1 in
    let m = (n + 15) / 16 in
    let a = Array1.create int16_unsigned c_layout m in
    begin
	for i = 0 to n0 / 16 - 1 do
	    Array1.set a i (Array1.get a0 i)
	done;
	if n0 mod 16 = 0 then
	    for i = 0 to m1 - 1 do
		Array1.set a (i + m0) (Array1.get a1 i)
	    done
	else begin
	    let jr = (16*m - n0) mod 16 in
	    let xL = Array1.get a0 (m0 - 1) in
	    let xH = Array1.get a1 0 in
	    let x = (xL lor (xH lsl (16 - jr))) land 0xffff in
	    Array1.set a (m0 - 1) x;
	    for i = m0 to m0 + m1 - 2 do
		let k = 16*i - n0 in
		let j = k / 16 in
		let xL = Array1.get a1 j in
		let xH = Array1.get a1 (j + 1) in
		let x = ((xL lsr jr) lor (xH lsl (16 - jr))) land 0xffff in
		Array1.set a i x
	    done;
	    if m0 + m1 - 1 < m then begin
		let xL = Array1.get a1 (m1 - 1) in
		Array1.set a (m - 1) (xL lsr jr)
	    end
	end;
	(a, n)
    end

let prefix n' (a, n) =
    if n' = n then (a, n) else
    let m' = (n' + 15) / 16 in
    let a' = Array1.create int16_unsigned c_layout m' in
    begin
	Array1.blit (Array1.sub a 0 m') a';
	if n' mod 16 > 0 then begin
	    let x = Array1.get a' (m' - 1) land ((1 lsl (n' mod 16)) - 1) in
	    Array1.set a' (m' - 1) x
	end;
	(a', n')
    end

let slice iL iU (a, n) =
    assert (0 <= iL && iL <= iU && iU <= n);
    if iL = iU then empty else
    if iL = 0 then prefix iU (a, n) else
    let n' = iU - iL in
    let m' = (n' + 15) / 16 in
    let a' = Array1.create int16_unsigned c_layout m' in
    if iL mod 16 = 0 then begin
	Array1.blit (Array1.sub a (iL / 16) m') a';
	if n' mod 16 > 0 then begin
	    let x = Array1.get a' (m' - 1) land ((1 lsl (n' mod 16)) - 1) in
	    Array1.set a' (m' - 1) x
	end;
	(a', n')
    end else begin
	let kL = iL / 16 in
	let jL = iL mod 16 in
	for k = 0 to m' - 2 do
	    let xL = Array1.get a (kL + k) in
	    let xH = Array1.get a (kL + k + 1) in
	    let x = ((xL lsr jL) lor (xH lsl (16 - jL))) land 0xffff in
	    Array1.set a' k x
	done;
	let k = m' - 1 in
	let xL = Array1.get a (kL + k) in
	let xH = if iU > 16*(kL+k+1) then Array1.get a (kL + k + 1) else 0 in
	let x = ((xL lsr jL) lor (xH lsl (16 - jL)))
		land (0xffff lsr (15 - (n' + 15) mod 16)) in
	Array1.set a' k x;
	(a', n')
    end

let common_prefix_length (a0, n0) (a1, n1) =
    let n = min n0 n1 in
    let m = (n + 15) / 16 in
    let rec loop i =
	if i = m then n else
	let x0, x1 = Array1.get a0 i, Array1.get a1 i in
	if x0 = x1 then loop (i + 1) else
	let x = x0 lxor x1 in
	min n (16*i + Int.bitcount ((x - 1) lxor x) - 1) in
    loop 0

let common_prefix b0 b1 = prefix (common_prefix_length b0 b1) b0
