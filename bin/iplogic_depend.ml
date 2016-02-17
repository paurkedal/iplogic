(* Copyright (C) 2014--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

open Iplogic_types
open Printf

let path_template_rex = Pcre.regexp "%[tc]"

let () =
  let opt_args = ref [] in
  let opt_split_chains = ref false in
  let opt_comp_o = ref "" in
  let opt_o = ref "" in
  let opt_incdirs = ref ["."] in
  let argusage = "iplogic-depend OPTIONS INPUT..." in
  let argspecs = Arg.align [
    "-I", Arg.String (fun dir -> opt_incdirs := dir :: !opt_incdirs),
      "DIR Prepend DIR to the list of directories to search for dependencies.";
    "-split-chains", Arg.Set opt_split_chains,
      " This should be passed iff it is passed to iplogic-compile.";
    "-comp-o", Arg.Set_string opt_comp_o,
      "PATTERN This should match the -o option of iplogic-compile.";
    "-o", Arg.Set_string opt_o,
      "OUTPUT Write dependencies to OUTPUT rather that standard output.";
  ] in
  Arg.parse argspecs (fun arg -> opt_args := arg :: !opt_args) argusage;
  let oc = if !opt_o = "" then stdout else open_out !opt_o in
  List.iter
    begin fun fp ->
      let input = Iplogic_lexer.dep_parse_file fp in
      if !opt_split_chains then
        List.iteri
          (fun idx -> function
            | Dep_chain (tn, chn) ->
              let f = function "%t" -> tn | "%c" -> chn | _ -> assert false in
              if idx > 0 then output_char oc ' ';
              output_string oc
                (Pcre.substitute ~rex:path_template_rex ~subst:f !opt_comp_o)
            | Dep_include _ -> ())
          input
      else
        output_string oc !opt_comp_o;
      output_char oc ':';
      List.iter
        (function
          | Dep_include vp ->
            let ifp = Iplogic_lexer.locate_file ~include_dirs:!opt_incdirs vp in
            output_char oc ' ';
            output_string oc ifp
          | Dep_chain (tn, chn) -> ())
        input;
      output_char oc '\n'
    end
    (List.rev !opt_args);
  if !opt_o <> "" then close_out oc
