(* Copyright (C) 2013  Petter Urkedal <paurkedal@gmail.com>
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

exception Invalid_substitution of string

let set_string r = Arg.String (fun arg -> r := Some arg)

let emit_rules_for_chain och (tn, chn, rules) =
  let commands = Iplogic_iptables.emit_chain (tn, chn) rules in
  Shell_monoid.output_shell_seq och commands

let template_rex = Pcre.regexp "@[A-Z]+@"

let emit_templated template_path och subst emit_rules =
  let ich = open_in template_path in
  try
    while true do
      let ln = input_line ich in
      if ln = "@RULES@" then
	emit_rules och
      else begin
	output_string och (Pcre.substitute ~rex:template_rex ~subst ln);
	output_char och '\n'
      end
    done
  with
  | End_of_file -> close_in ich
  | xc -> close_in ich; raise xc

let bad_subst x = eprintf "warning: No substitution for %s.\n" x; x

let emit_monolithic ?template_path och chains =
  match template_path with
  | None -> List.iter (emit_rules_for_chain och) chains
  | Some tp ->
    emit_templated tp och bad_subst
	(fun och -> List.iter (emit_rules_for_chain och) chains)

let path_template_rex = Pcre.regexp "%[tc]"

let emit_by_chain ?template_path path_template =
  List.iter
    (fun (tn, chn, rules) ->
      let subst_path = function "%t" -> tn | "%c" -> chn | _ -> assert false in
      let subst = function
	| "@TABLE@" -> tn
	| "@CHAIN@" -> chn
	| x -> bad_subst x in
      let fp = Pcre.substitute ~rex:path_template_rex ~subst:subst_path
			       path_template in
      let och = open_out fp in
      begin match template_path with
      | None ->
	emit_rules_for_chain och (tn, chn, rules)
      | Some tp ->
	emit_templated tp och subst
	    (fun och -> emit_rules_for_chain och (tn, chn, rules))
      end;
      close_out och)

let () =
  let opt_o = ref None in
  let opt_split_chains = ref false in
  let opt_args = ref [] in
  let opt_template = ref None in
  let argusage = "iplogic-compile [-o PATH] INPUT" in
  let argspecs = Arg.align [
    "-o", set_string opt_o,
      "PATH Store firewall stript in PATH.  \
	    If --split-chains has been passed, then this is taken to be a \
	    template for the individual filewall scripts substituting \
	    the table name for %t and \
	    the chain name for %c.";
    "-template", set_string opt_template,
      "PATH Wrap each output file in the template file PATH, substituting \
	    the table name for @TABLE@, \
	    the chain name for @CHAIN@, and \
	    the rules for a line containing exactly the string @RULES@.";
    "-split-chains", Arg.Set opt_split_chains,
      " Split chains into individual files.";
  ] in
  let misusage msg = eprintf "%s\n" msg; Arg.usage argspecs argusage; exit 64 in
  Arg.parse argspecs (fun arg -> opt_args := arg :: !opt_args) argusage;
  let input_path =
    match !opt_args with
    | [arg] -> arg
    | _ -> misusage "Expecting a single positional argument." in
  let input = Iplogic_lexer.parse_file input_path in
  let output = Iplogic_passes.compile input in
  match !opt_split_chains, !opt_o with
  | false, None ->
    emit_monolithic ?template_path:!opt_template stdout output
  | false, Some path ->
    let och = open_out path in
    emit_monolithic ?template_path:!opt_template och output;
    close_out och
  | true, None ->
    misusage "An output directory must be specified when splitting the output."
  | true, Some path_template ->
    emit_by_chain ?template_path:!opt_template path_template output
