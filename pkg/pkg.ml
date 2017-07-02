#! /usr/bin/env ocaml
#use "topfind"
#require "topkg-jbuilder"

open Topkg

let licenses = [Pkg.std_file "COPYING"]

let () = Topkg_jbuilder.describe ~licenses ~build ()
