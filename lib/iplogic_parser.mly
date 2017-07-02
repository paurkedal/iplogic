/* Copyright (C) 2012--2017  Petter A. Urkedal <paurkedal@gmail.com>
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
open Iplogic_types

let lhs_loc () = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())
let rhs_loc i = (Parsing.rhs_start_pos i, Parsing.rhs_end_pos i)

let parse_error s =
  Iplogic_diag.eprint_loc (lhs_loc ());
  prerr_string s; prerr_newline ()

let default_table = ref "filter"
%}

%token<string -> Iplogic_types.def list> INCLUDE
%token VAL CON CHAIN POLICY IF CONTINUE FAIL RETURN CALL GOTO LOG
%token ACCEPT REJECT DROP ALTER
%token<string> NAME FLAG STRING STRING_START STRING_MID STRING_END
%token<Iplogic_types.value> VALUE
%token<Iplogic_types.expr> EXPR
%token<bool> BOOL
%token EOF LPAREN RPAREN
%token COLON IS NOT OR UNION INTER COMPL DOTS
%left NOT
%left OR
%left UNION COMPL
%left INTER

%type<Iplogic_types.def list> start
%type<Iplogic_types.dep list> dep_start
%start start dep_start
%%

start: decls EOF { List.rev $1 };
dep_start: dep_decls EOF { List.rev $1 };

decls:
    /* empty */ { [] }
  | decls VAL NAME IS vexpr { Def_val (lhs_loc (), $3, $5) :: $1 }
  | decls VAL NAME COLON vtype { Def_val_type (lhs_loc (), $3, $5) :: $1 }
  | decls CON NAME IS condition { Def_cond (lhs_loc (), $3, $5) :: $1 }
  | decls CHAIN NAME NAME policy predicate
    { Def_chain (lhs_loc (), $3, $4, $5, $6) :: $1 }
  | decls CHAIN NAME policy predicate
    { Def_chain (lhs_loc (), !default_table, $3, $4, $5) :: $1 }
  | decls error { $1 }
  | decls INCLUDE STRING { List.rev_append ($2 $3) $1 }
  ;
dep_decls:
    /* empty */ { [] }
  | dep_decls VAL NAME IS vexpr { $1 }
  | dep_decls VAL NAME COLON vtype { $1 }
  | dep_decls CON NAME IS condition { $1 }
  | dep_decls CHAIN NAME NAME policy predicate
    { Dep_chain ($3, $4) :: $1 }
  | dep_decls CHAIN NAME policy predicate
    { Dep_chain (!default_table, $3) :: $1 }
  | dep_decls error { $1 }
  | dep_decls INCLUDE STRING { Dep_include $3 :: $1 }
  ;

policy: /* empty */ { Policy_none } | POLICY policy_arg { $2 };
policy_arg:
    ACCEPT { Policy_accept }
  | DROP { Policy_drop }
  ;

condition:
    conjunction { $1 }
  | condition OR conjunction { Cond_or (lhs_loc (), $1, $3) }
  ;
conjunction:
    atomic_condition { $1 }
  | conjunction atomic_condition { Cond_and (lhs_loc (), $1, $2) }
  ;
atomic_condition:
    NOT atomic_condition { Cond_not (lhs_loc (), $2) }
  | FLAG vexpr { Cond_flag (lhs_loc (), $1, $2) }
  | LPAREN condition RPAREN { $2 }
  | NAME { Cond_call (lhs_loc (), $1) }
  | BOOL { Cond_const (lhs_loc (), $1) }
  ;

vtype: NAME { $1 };

vexpr:
    atomic_vexpr { $1 }
  | UNION vexpr { $2 }
  | vexpr UNION vexpr { Expr_union (lhs_loc (), $1, $3) }
  | vexpr INTER vexpr { Expr_isecn (lhs_loc (), $1, $3) }
  | vexpr COMPL vexpr { Expr_compl (lhs_loc (), $1, $3) }
  ;

string_mid:
  | vexpr { [$1] }
  | string_mid STRING_MID vexpr
    { $3 :: Expr_value (lhs_loc (), Value_string $2) :: $1 }
  ;

atomic_vexpr:
    EXPR { $1 }
  | VALUE { Expr_value (lhs_loc (), $1) }
  | VALUE DOTS VALUE { Expr_range (lhs_loc (), $1, $3) }
  | STRING { Expr_value (lhs_loc (), Value_string $1) }
  | STRING_START string_mid STRING_END
    { let s1 = Expr_value (rhs_loc 1, Value_string $1) in
      let s3 = Expr_value (rhs_loc 3, Value_string $3) in
      Expr_cat (lhs_loc (), s1 :: (List.rev (s3 :: $2))) }
  | NAME { Expr_var (lhs_loc (), $1) }
  ;

predicate:
    ACCEPT { Chain_decision (lhs_loc (), Accept) }
  | REJECT { Chain_decision (lhs_loc (), Reject) }
  | DROP { Chain_decision (lhs_loc (), Drop) }
  | ALTER NAME options { Chain_decision (lhs_loc (), Alter ($2, List.rev $3)) }
  | CONTINUE { Chain_continue (lhs_loc ()) }
  | RETURN { Chain_return (lhs_loc ()) }
  | FAIL { Chain_fail (lhs_loc ()) }
  | GOTO NAME { Chain_goto (lhs_loc (), $2) }
  | CALL NAME predicate { Chain_call (lhs_loc (), $2, $3) }
  | LOG options predicate { Chain_log (lhs_loc (), List.rev $2, $3) }
  | IF condition predicate predicate { Chain_if (lhs_loc (), $2, $3, $4) }
  ;

options:
    /* empty */ { [] }
  | options FLAG vexpr { ($2, $3) :: $1 }
  ;
