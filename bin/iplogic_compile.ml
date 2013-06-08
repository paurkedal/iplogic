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

let set_string r = Arg.String (fun arg -> r := Some arg)

let emit_chain och (tn, chn, chain) =
  let commands = Iplogic_iptables.emit_chain (tn, chn) chain in
  Shell_monoid.output_shell_seq och commands

let emit_monolithic och = List.iter (emit_chain och)

let path_template_rex = Pcre.regexp "%[tc]"

let emit_by_chain path_template =
  List.iter
    (fun (tn, chn, chain) ->
      let subst = function "%t" -> tn | "%c" -> chn | _ -> assert false in
      let fp = Pcre.substitute ~rex:path_template_rex ~subst path_template in
      let och = open_out fp in
      emit_chain och (tn, chn, chain))

let () =
  let opt_o = ref None in
  let opt_split_chains = ref false in
  let opt_args = ref [] in
  let argusage = "iplogic-to-iptables [-o PATH] INPUT" in
  let argspecs = Arg.align [
    "-o", set_string opt_o,
      "PATH Store firewall stript in PATH.  \
	    If --split-chains has been passed, then this is taken to be a \
	    template for the individual filewall scripts substituting \
	    the table name for %t and \
	    the chain name for %c.";
    "--split-chains", Arg.Set opt_split_chains,
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
  | false, None -> emit_monolithic stdout output
  | false, Some path ->
    let och = open_out path in emit_monolithic och output; close_out och
  | true, None ->
    misusage "An output directory must be specified when splitting the output."
  | true, Some path_template -> emit_by_chain path_template output
