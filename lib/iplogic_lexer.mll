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

{
open Iplogic_address
open Iplogic_types
open Iplogic_parser
open Printf
open Unprime_list

let get_loc lexbuf = (lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)

type paren = Paren | Interp

type state = {
  mutable s_nesting : paren list;
  s_parse_file : string -> def list;
}

let parse_error lexbuf s =
  Iplogic_diag.eprint_loc (get_loc lexbuf);
  prerr_string s; prerr_newline ()

let next_line lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum;
  }

let unquote s = s

let keywords =
  let entries = [
    "include", INCLUDE (fun _ -> assert false);
    "val", VAL;
    "con", CON;
    "is", IS;
    "chain", CHAIN;
    "policy", POLICY;

    "call", CALL;
    "goto", GOTO;
    "log", LOG;
    "if", IF;
    "continue", CONTINUE;
    "return", RETURN;
    "fail", FAIL;

    "accept", ACCEPT;
    "reject", REJECT;
    "drop", DROP;
    "alter", ALTER;

    "true", BOOL true;
    "false", BOOL false;
  ] in
  let ht = Hashtbl.create (List.length entries) in
  List.iter (fun (kw, tk) -> Hashtbl.add ht kw tk) entries;
  ht
}

let alpha = ['a'-'z' 'A'-'Z']
let alnum = ['a'-'z' 'A'-'Z' '0'-'9']
let digit = ['0'-'9']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let flagident = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '-' '0'-'9']*
let ipv6comp = hexdigit (hexdigit (hexdigit hexdigit?)?)?
let ipv4addr = digit+ '.' digit+ '.' digit+ '.' digit+
let ipv6addr =
        ipv6comp ':' ipv6comp ':' ipv6comp ':' ipv6comp ':'
        ipv6comp ':' ipv6comp ':' ipv6comp ':' ipv6comp
      | ipv6comp ':' ipv6comp ':' ipv6comp ':' ipv6comp ':'
        ipv6comp ':' ipv6comp ':' digit+ '.' digit+ '.' digit+ '.' digit+
      | (ipv6comp (':' ipv6comp)*)? ':'
            (':' | (':' ipv6comp)+ | (':' ipv6comp)* (':' ipv4addr))
let dnslabel = alnum+ ('-'+ alnum+)*
let dnslabel_tld = alpha alnum* ('-'+ alnum+)*
let dnsdomain = (dnslabel '.')+ dnslabel_tld
let stringdata = ([^ '\\' '"' '$'] | '\\' _)*

rule lexmain state = parse
  | '\n' { next_line lexbuf; lexmain state lexbuf }
  | '#' [^ '\n']* | [' ' '\t'] { lexmain state lexbuf }
  | ipv6addr ('/' digit+)? as s
    { try VALUE (Value_ipaddrs (ipaddrs_of_string s)) with
      Invalid_argument msg -> parse_error lexbuf msg; lexmain state lexbuf }
  | ipv4addr ('/' digit+)? as s
    { try VALUE (Value_ipaddrs (ipaddrs_of_string s)) with
      Invalid_argument msg -> parse_error lexbuf msg; lexmain state lexbuf }
  | dnsdomain as s { VALUE (Value_dnsname s) }
  | digit+ as s { VALUE (Value_int (int_of_string s)) }
  | '"' (stringdata as s) '"' { STRING (unquote s) }
  | '"' (stringdata as s) "$("
    { state.s_nesting <- Interp :: state.s_nesting; STRING_START (unquote s) }
  | '(' { state.s_nesting <- Paren :: state.s_nesting; LPAREN }
  | ')'
    { match state.s_nesting with
      | Paren :: nesting -> state.s_nesting <- nesting; RPAREN
      | Interp :: nesting -> state.s_nesting <- nesting; lexinterp state lexbuf
      | [] -> parse_error lexbuf "Unmatched closing paranthesis.";
              lexmain state lexbuf }
  | '!' { NOT } | "or" { OR }
  | '\\' { COMPL } | '&' { INTER } | ',' { UNION }
  (*| '=' | "<=" | ">=" | "<" | ">" as op { R op }*)
  | '-' alnum | "--" flagident as s { FLAG s }
  | ident ':' digit+ as s { NAME s }
  | ident as s
    { try
        match Hashtbl.find keywords s with
        | INCLUDE f -> INCLUDE state.s_parse_file
        | tok -> tok
      with Not_found -> NAME s }
  | ':' { COLON }
  | ".." { DOTS }
  | eof { EOF }
  | _ as c
    { parse_error lexbuf (sprintf "Lexical error at '%c'." c);
      lexmain state lexbuf }

and lexinterp state = parse
  | (stringdata as s) "$("
    { state.s_nesting <- Interp :: state.s_nesting; STRING_MID (unquote s) }
  | (stringdata as s) '"'
    { STRING_END (unquote s) }

{
open Lexing

let locate_file ~include_dirs path =
  match
    List.search
      (fun dir ->
        try let fp = Filename.concat dir path in
            ignore (Unix.stat fp); Some fp
        with Unix.Unix_error (Unix.ENOENT, _, _) -> None)
      include_dirs
  with
  | Some fp -> fp
  | None ->
    ksprintf (fun s -> raise (Sys_error s)) "Cannot find %s in include dirs: %s"
      path (String.concat ", " include_dirs)

let rec parse_file ?(include_dirs = ["."]) path =
  let path = if Filename.is_relative path then locate_file ~include_dirs path
                                          else path in
  let chan = open_in path in
  try
    let lexbuf = from_channel chan in
    lexbuf.lex_curr_p <- {
      pos_fname = path;
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0;
    };
    let state = {
      s_nesting = [];
      s_parse_file = parse_file ~include_dirs;
    } in
    let r = start (lexmain state) lexbuf in
    close_in chan; r
  with xc ->
    close_in chan;
    raise xc

let rec dep_parse_file path =
  let chan = open_in path in
  try
    let lexbuf = from_channel chan in
    let state = {
      s_nesting = [];
      s_parse_file = fun _ -> assert false;
    } in
    let r = dep_start (lexmain state) lexbuf in
    close_in chan; r
  with xc ->
    close_in chan;
    raise xc
}
