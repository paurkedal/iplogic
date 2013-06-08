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

{
open Iplogic_address
open Iplogic_types
open Iplogic_parser
open Printf

let get_loc () = Parsing.symbol_start_pos ()

let parse_error lexbuf s =
    let loc = lexbuf.Lexing.lex_curr_p in
    eprintf "%s:%d,%d: %s\n"
	    loc.Lexing.pos_fname
	    loc.Lexing.pos_lnum
	    (loc.Lexing.pos_cnum - loc.Lexing.pos_bol)
	    s

let next_line lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
	Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
	Lexing.pos_bol = pos.Lexing.pos_cnum;
    }

let keywords =
    let entries = [
        "val", VAL;
	"con", CON;
	"is", IS;
	"chain", CHAIN;

	"call", CALL;
	"goto", GOTO;
	"log", LOG;
        "if", IF;
	"return", RETURN;
	"fail", FAIL;

	"accept", DECISION Accept;
	"reject", DECISION Reject;
	"drop", DECISION Drop;

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

rule lexfunc = parse
  | '\n' { next_line lexbuf; lexfunc lexbuf }
  | '#' [^ '\n']* | [' ' '\t'] { lexfunc lexbuf }
  | ipv6addr ('/' digit+)? as s { VALUE (Value_ipaddrs (ipaddrs_of_string s)) }
  | ipv4addr ('/' digit+)? as s { VALUE (Value_ipaddrs (ipaddrs_of_string s)) }
  | dnsdomain as s { VALUE (Value_dnsname s) }
  | digit+ as s { VALUE (Value_int (int_of_string s)) }
  | '"' (([^ '\\' '"'] | '\\' _)* as s) '"' { VALUE (Value_string s) }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '!' { NOT } | "or" { OR }
  | '\\' { COMPL } | '&' { INTER } | ',' { UNION }
  (*| '=' | "<=" | ">=" | "<" | ">" as op { R op }*)
  | '-' alnum | "--" ident as s { FLAG s }
  | ident ':' digit+ as s { NAME s }
  | ident as s { try Hashtbl.find keywords s with Not_found -> NAME s }
  | ':' { COLON }
  | ".." { DOTS }
  | eof { EOF }
  | _ as c
    {
        parse_error lexbuf (sprintf "Lexical error at '%c'." c);
	lexfunc lexbuf
    }

{
open Lexing

let rec parse lexbuf = start lexfunc lexbuf

let parse_file path =
    let chan = open_in path in
    try
	let lexbuf = from_channel chan in
	lexbuf.lex_curr_p <- {
	    pos_fname = path;
	    pos_lnum = 1;
	    pos_bol = 0;
	    pos_cnum = 0;
	};
	parse lexbuf
    with xc ->
	close_in chan;
	raise xc
}
