#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let mirage = Conf.with_pkg ~default:false "mirage"

let () =
  let opams =
    [ Pkg.opam_file "opam" ~lint_deps_excluding:(Some ["ppx_tools"; "io-page"; "ipaddr"]) ]
  in
  Pkg.describe ~opams "telnet" @@ fun c ->
  let mirage = Conf.value c mirage in
  Ok [
    Pkg.mllib ~api:["Telnet"] "src/telnet.mllib";
    Pkg.mllib ~cond:mirage "mirage/telnet_mirage.mllib" ~dst_dir:"mirage/"
  ]
