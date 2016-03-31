#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let mirage = Env.bool "mirage-types-lwt"
let () = Pkg.describe "telnet" ~builder:(`OCamlbuild []) [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/telnet";
    Pkg.lib ~cond:mirage ~exts:Exts.module_library "mirage/telnet_mirage";
    Pkg.doc "README.md"; ]
