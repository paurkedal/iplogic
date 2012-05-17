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
 * [i] of the bitstring is stored in bit number [(16 - i) mod 16] or [i mod
 * 16] of [a.(i / 16]). *)
type t = (int, int16_unsigned_elt, c_layout) Array1.t * int

let word_get j x = x lsr (15 - j) land 1 <> 0
let word_set j c x = if c then x lor 1 lsl (15 - j) else x
let word_get8 j8 x = x lsr (8 * (1 - j8)) land 255
let word_of8 x y = x lsl 8 lor y
let word_prefix j x = x land 0xffff lsl (16 - j)
let word_suffix j x = x land (1 lsl (16 - j) - 1)
let word_widen j x = x lsl j
let word_cat    j x0 x1 = (x0       lor x1 lsr (16 - j)) land 0xffff
let word_recomb j x0 x1 = (x0 lsl j lor x1 lsr (16 - j)) land 0xffff
let word_firstbit x = 15 - Int.floor_log2 x

(*
let word_get j x = x lsr j land 1 <> 0
let word_set j c x = if c then x lor 1 lsl j else x
let word_get8 j8 x = x lsr (8 * j8) land 255
let word_of8 x y = x lor y lsl 8
let word_prefix j x = x land (1 lsl j - 1)
let word_suffix j x = x lsr j		(* Suffix from bit j. *)
let word_widen j x = x			(* Add j bits to the last word. *)
let word_cat    j x0 x1 = (x0       lor x1 lsl (16 - j)) land 0xffff
let word_recomb j x0 x1 = (x0 lsr j lor x1 lsl (16 - j)) land 0xffff
let word_firstbit x = Int.bitcount ((x - 1) lxor x) - 1
*)

let empty = (Array1.create int16_unsigned c_layout 0, 0)

let length (_, n) = n
let length8 (_, n) = (n + 7) / 8
let length16 (_, n) = (n + 15) / 16

let get i (a, _) = word_get (i mod 16) (Array1.get a (i / 16))
let get8 i (a, _) = word_get8 (i mod 2) (Array1.get a (i / 2))
let get16 i (a, _) = Array1.get a i

let init16 n f =
    let a = Array1.create int16_unsigned c_layout n in
    for i = 0 to n - 1 do Array1.set a i (f i) done;
    (a, n * 16)

let init8 n f =
    let a = Array1.create int16_unsigned c_layout ((n + 1) / 2) in
    for i = 0 to n / 2 - 1 do
	Array1.set a i (word_of8 (f (2*i)) (f (2*i + 1)))
    done;
    if n mod 2 > 0 then Array1.set a (n / 2) (word_of8 (f (n - 1)) 0);
    (a, n * 8)

let init n f =
    let m = (n + 15) / 16 in
    let a = Array1.create int16_unsigned c_layout m in
    let ilast = n / 16 in
    let jlast = n mod 16 in
    let rec mk16 i j jmax h =
        if j = jmax then h else
        mk16 i (j + 1) jmax (word_set j (f (16*i + j)) h) in
    for i = 0 to ilast - 1 do
	Array1.set a i (mk16 i 0 16 0) done;
    if jlast <> 0 then Array1.set a ilast (mk16 ilast 0 jlast 0);
    (a, n)

let of_list16 xs =
    let m = List.length xs in
    let a = Array1.create int16_unsigned c_layout m in
    let xs_r = ref xs in
    for i = 0 to m - 1 do
	match !xs_r with
	  | x :: xs -> Array1.set a i x; xs_r := xs
	  | [] -> assert false
    done;
    assert (!xs_r = []);
    (a, 16*m)

let of_list8 xs =
    let m8 = List.length xs in
    let m = (m8 + 1) / 2 in
    let a = Array1.create int16_unsigned c_layout m in
    let xs_r = ref xs in
    for i = 0 to m - 1 do
	match !xs_r with
	  | x0 :: x1 :: xs ->
	    Array1.set a i (word_of8 x0 x1);
	    xs_r := xs
	  | [x0] ->
	    Array1.set a i (word_of8 x0 0);
	    xs_r := []
	  | [] -> assert false
    done;
    assert (!xs_r = []);
    (a, 8*m8)

let of_list xs =
    let xs_r = ref xs in
    init (List.length xs)
	begin fun _ ->
	    match !xs_r with
	      | x :: xs -> xs_r := xs; x
	      | [] -> assert false
	end

let singleton_false = init 1 (konst false)
let singleton_true  = init 1 (konst true)
let singleton = function false -> singleton_false | true -> singleton_true

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
	    accu_x := f (16*i + j) (word_get j x) !accu_x
	done
    done;
    let m_rem = n mod 16 in
    if m_rem > 0 then begin
	let i = n / 16 in
	let x = Array1.get a (n / 16) in
	for j = 0 to m_rem - 1 do
	    accu_x := f (16*i + j) (word_get j x) !accu_x
	done
    end;
    !accu_x

let foldi8 f (a, n) accu =
    let accu_x = ref accu in
    let m8 = (n + 7) / 8 in
    for i = 0 to m8 / 2 - 1 do
	let x = Array1.get a i in
	accu_x := f (2*i) (word_get8 0 x) !accu_x;
	accu_x := f (2*i + 1) (word_get8 1 x) !accu_x
    done;
    if m8 mod 2 > 0 then begin
	let i = m8 / 2 in
	accu_x := f (2*i) (word_get8 0 (Array1.get a i)) !accu_x
    end;
    !accu_x

let foldi16 f (a, n) accu =
    let accu_x = ref accu in
    for i = 0 to (n + 15) / 16 - 1 do
	accu_x := f i (Array1.get a i) !accu_x
    done;
    !accu_x

let iteri f b = foldi (fun i x () -> f i x) b ()
let iteri8 f b = foldi8 (fun i x () -> f i x) b ()
let iteri16 f b = foldi16 (fun i x () -> f i x) b ()

let cat (a0, n0) (a1, n1) =
    if n1 = 0 then (a0, n0) else
    if n0 = 0 then (a1, n1) else
    let m0 = (n0 + 15) / 16 in
    let m1 = (n1 + 15) / 16 in
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
	    let x = word_cat jr xL xH in
	    Array1.set a (m0 - 1) x;
	    for i = m0 to m0 + m1 - 2 do
		let k = 16*i - n0 in
		let j = k / 16 in
		let xL = Array1.get a1 j in
		let xH = Array1.get a1 (j + 1) in
		let x = word_recomb jr xL xH in
		Array1.set a i x
	    done;
	    if m0 + m1 - 1 < m then begin
		let xL = Array1.get a1 (m1 - 1) in
		Array1.set a (m - 1) (word_widen jr (word_suffix jr xL))
	    end
	end;
	(a, n)
    end

let has_prefix (aP, nP) (a, n) =
    if nP > n then false else
    let mP = nP / 16 in
    let rec loop16 k =
	if k = mP then true else
	if Array1.get aP k <> Array1.get a k then false else
	loop16 (k + 1) in
    if not (loop16 0) then false else
    if 16*mP = nP then true else
    word_prefix (nP mod 16) (Array1.get aP mP lxor Array1.get a mP) = 0

let prefix n' (a, n) =
    if n' = n then (a, n) else
    let m' = (n' + 15) / 16 in
    let a' = Array1.create int16_unsigned c_layout m' in
    begin
	Array1.blit (Array1.sub a 0 m') a';
	if n' mod 16 > 0 then begin
	    let x = Array1.get a' (m' - 1) in
	    Array1.set a' (m' - 1) (word_prefix (n' mod 16) x)
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
	    let x = Array1.get a' (m' - 1) in
	    Array1.set a' (m' - 1) (word_prefix (n' mod 16) x)
	end;
	(a', n')
    end else begin
	let kL = iL / 16 in
	let jL = iL mod 16 in
	for k = 0 to m' - 2 do
	    let xL = Array1.get a (kL + k) in
	    let xH = Array1.get a (kL + k + 1) in
	    let x = word_recomb jL xL xH in
	    Array1.set a' k x
	done;
	let k = m' - 1 in
	let xL = Array1.get a (kL + k) in
	let xH = if iU > 16*(kL+k+1) then Array1.get a (kL + k + 1) else 0 in
	let x = word_prefix ((n' + 15) mod 16 + 1) (word_recomb jL xL xH) in
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
	min n (16*i + word_firstbit x) in
    loop 0

let common_prefix b0 b1 = prefix (common_prefix_length b0 b1) b0

let common_prefix_length_from i0 b0 i1 b1 =
    if i0 = 0 && i1 = 0 then common_prefix_length b0 b1 else
    (* TODO: Can be optimized, though it'll take some coding for unaligned
     * cases. *)
    let i_max = min (length b0 - i0) (length b1 - i1) in
    let rec loop i =
	if i >= i_max || get (i + i0) b0 <> get (i + i1) b1 then i else
	loop (i + 1) in
    loop 0

let to_string b =
    if length b = 0 then "[]" else
    let buf = Buffer.create (length b) in
    iteri (fun i x -> Buffer.add_char buf (if x then '1' else '0')) b;
    Buffer.contents buf

let of_string s =
    init (String.length s)
	begin fun i ->
	    match s.[i] with
	    | '0' -> false
	    | '1' -> true
	    | _ -> invalid_arg "Bitstring.of_string expects a sequence of \
				zeroes and ones."
	end