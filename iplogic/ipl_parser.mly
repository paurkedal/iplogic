/* Copyright (C) 2012  Petter Urkedal <paurkedal@gmail.com>
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
 */

%{
open Ipl_types
open Printf

let get_loc () = Parsing.symbol_start_pos ()

let parse_error s =
    let loc0 = Parsing.symbol_start_pos () in
    let loc1 = Parsing.symbol_end_pos () in
    eprintf "%s:%d,%d-%d,%d: %s\n"
	    loc0.Lexing.pos_fname
	    loc0.Lexing.pos_lnum (loc0.Lexing.pos_cnum - loc0.Lexing.pos_bol)
	    loc1.Lexing.pos_lnum (loc1.Lexing.pos_cnum - loc1.Lexing.pos_bol)
	    s
%}

%token VAL CON CHAIN IF FAIL RETURN CALL GOTO LOG
%token<Ipl_types.decision> DECISION
%token<string> NAME FLAG
%token<Ipl_types.value> VALUE
%token EOF LPAREN RPAREN
%token COLON IS NOT OR UNION INTER COMPL DOTS
%left NOT
%left OR
%left UNION COMPL
%left INTER

%type<Ipl_types.def list> start
%start start
%%

start: decls EOF { List.rev $1 };

decls:
    /* empty */ { [] }
  | decls VAL NAME IS vexpr { Def_val (get_loc (), $3, $5) :: $1 }
  | decls VAL NAME COLON vtype { Def_val_type (get_loc (), $3, $5) :: $1 }
  | decls CON NAME IS condition { Def_cond (get_loc (), $3, $5) :: $1 }
  | decls CHAIN NAME predicate { Def_chain (get_loc (), $3, $4) :: $1 }
  | decls error { $1 }
  ;

condition:
    conjunction { $1 }
  | condition OR conjunction { Cond_or (get_loc (), $1, $3) }
  ;
conjunction:
    atomic_condition { $1 }
  | conjunction atomic_condition { Cond_and (get_loc (), $1, $2) }
  ;
atomic_condition:
    NOT atomic_condition { Cond_not (get_loc (), $2) }
  | FLAG vexpr { Cond_flag (get_loc (), $1, $2) }
  | LPAREN condition RPAREN { $2 }
  | NAME { Cond_call (get_loc (), $1) }
  ;

vtype: NAME { $1 };

vexpr:
    atomic_vexpr { $1 }
  | UNION vexpr { $2 }
  | vexpr UNION vexpr { Expr_union (get_loc (), $1, $3) }
  | vexpr INTER vexpr { Expr_isecn (get_loc (), $1, $3) }
  | vexpr COMPL vexpr { Expr_compl (get_loc (), $1, $3) }
  ;
atomic_vexpr:
  | VALUE { Expr_value (get_loc (), $1) }
  | VALUE DOTS VALUE { Expr_range (get_loc (), $1, $3) }
  | NAME { Expr_var (get_loc (), $1) }
  ;

predicate:
    DECISION { Chain_decision (get_loc (), $1) }
  | RETURN { Chain_return (get_loc ()) }
  | FAIL { Chain_fail (get_loc ()) }
  | GOTO NAME { Chain_goto (get_loc (), $2) }
  | CALL NAME predicate { Chain_call (get_loc (), $2, $3) }
  | LOG options predicate { Chain_log (get_loc (), $2, $3) }
  | IF condition predicate predicate { Chain_if (get_loc (), $2, $3, $4) }
  ;

options:
    /* empty */ { [] }
  | options FLAG vexpr { ($2, $3) :: $1 }
  ;
