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

type shell_args =
   | AQ of string
   | AV of string
   | AL of shell_args list

type shell_seq =
   | SC of shell_args
   | SL of shell_seq list
   | SLor of shell_seq list

(** Puts a string in double quotes after escaping shell-special characters
    ['\\'], ['"'], ['`'], and ['$']. *)
let shell_quoted s =
  let buf = Buffer.create (String.length s + 8) in
  let put ch =
    begin match ch with
    | '\\' | '"' | '`' | '$' -> Buffer.add_char buf '\\'
    | _ -> ()
    end;
    Buffer.add_char buf ch in
  Buffer.add_char buf '"';
  String.iter put s;
  Buffer.add_char buf '"';
  Buffer.contents buf

let rec pop_first_shell_arg = function
  | AQ _ | AV _ as a -> Some (a, AL[])
  | AL[] -> None
  | AL (arg :: args) ->
    begin match pop_first_shell_arg arg with
    | None -> pop_first_shell_arg (AL args)
    | Some (arg0, AL[]) -> Some (arg0, AL (args))
    | Some (arg0, arg') -> Some (arg0, AL (arg' :: args))
    end

let rec output_shell_args' sep chan = function
  | AQ s -> output_string chan sep; output_string chan (shell_quoted s)
  | AV s -> output_string chan sep; output_string chan s
  | AL cs -> List.iter (output_shell_args' sep chan) cs

let output_shell_args chan args =
  match pop_first_shell_arg args with
  | None -> ()
  | Some (arg, args) ->
    output_shell_args' ""  chan arg;
    output_shell_args' " " chan args

let rec output_shell_seq_inline ~prec chan = function
  | SC c -> output_shell_args chan c
  | SL [] -> output_string chan ":"
  | SL [sq] | SLor [sq] -> output_shell_seq_inline ~prec chan sq
  | SL (sq :: sqs) ->
    output_string chan "{ ";
    List.iter
      (fun sq -> output_shell_seq_inline ~prec:1 chan sq;
                 output_string chan "; ") sqs;
    output_string chan "}"
  | SLor [] -> output_string chan "false"
  | SLor (sq :: sqs) ->
    output_shell_seq_inline ~prec:2 chan sq;
    List.iter
      (fun sq -> output_string chan " || ";
                 output_shell_seq_inline ~prec:2 chan sq) sqs

let rec output_shell_seq ?(prefix = "") chan = function
  | SL sq ->
    List.iter (output_shell_seq ~prefix chan) sq
  | sq ->
    output_string chan prefix;
    output_shell_seq_inline ~prec:0 chan sq;
    output_char chan '\n'
