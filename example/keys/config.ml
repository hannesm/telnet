open Mirage

let main = foreign "Unikernel.Main" (console @-> stackv4 @-> job)

let stack = generic_stackv4 default_console tap0

let () =
  add_to_opam_packages [ "telnet" ; "notty" ] ;
  add_to_ocamlfind_libraries [ "telnet"; "telnet.mirage" ; "notty" ; "notty.mirage" ; "notty.unix" ] ;
  register "network" [
    main $ default_console $ stack
  ]
