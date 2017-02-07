#! /usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let licenses = [Pkg.std_file "COPYING"]

(* Workaround for iplogic_parser.mli being deleted during native build. *)
let build_cmd c os targets =
  let ocamlbuild = Conf.tool "ocamlbuild" os in
  let build_dir = Conf.build_dir c in
  OS.Cmd.run @@
    Cmd.(ocamlbuild
          % "-use-ocamlfind" % "-build-dir" % build_dir % "-classic-display"
          %% of_list ("iplogic_parser.mli" :: targets))
let build = Pkg.build ~cmd:build_cmd ()

let load_modules fpath =
  OS.File.read fpath >>| fun content ->
  content
    |> String.split_on_char '\n'
    |> List.map String.trim
    |> List.filter ((<>) "")

let () = Pkg.describe ~licenses ~build "iplogic" @@ fun c ->
  load_modules "doc/api.odocl" >>= fun api ->
  Ok [
    Pkg.mllib ~api "lib/iplogic.mllib";
    Pkg.bin ~dst:"iplogic-compile" "bin/iplogic_compile";
    Pkg.bin ~dst:"iplogic-depend" "bin/iplogic_depend";
    Pkg.test ~run:false "tests/simplify_cidrs";
  ]
