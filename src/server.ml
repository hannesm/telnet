open Wire

type cmd = [ `WILL | `WILL_NOT | `DO | `DO_NOT ]

let cmd_to_string = function
  | `WILL -> "will"
  | `WILL_NOT -> "will not"
  | `DO -> "do"
  | `DO_NOT -> "do not"

type status =
  | Data
  | Command
  | Option of cmd
  | Suboption
  | EatSub of int * telnet_option * int list
  | SubDone

let status_to_string = function
  | Data -> "data"
  | Command -> "cmd"
  | Option cmd -> "option " ^ cmd_to_string cmd
  | Suboption -> "sub"
  | EatSub (n, _, col) -> Printf.sprintf "eatsub %d left, %d collected" n (List.length col)
  | SubDone -> "sub done"

type option_state = [
  | `Requested (* we requested *)
  | `Denied (* other side denied *)
  | `Accepted (* other side accepted *)
]

let option_state_to_string = function
  | `Requested -> "requested"
  | `Denied -> "denied"
  | `Accepted -> "accepted"

type state = {
  machina : status ;
  server_config : (cmd * telnet_option) list ;
  client_options : (option_state * telnet_option) list ;
}

let option_to_string (o, t) =
  Printf.sprintf "%s %s" (option_state_to_string o) (telnet_option_to_string t)

let state_to_string s =
  Printf.sprintf "state %s options %s\n"
    (status_to_string s.machina)
    (String.concat ", " (List.map option_to_string s.client_options))

let client_option_state s option =
  try Some (fst (List.find (fun (_, o) -> o = option) s.client_options))
  with Not_found -> None

let emit_cmd cmd =
  let b = match cmd with
  | `WILL -> WILL
  | `WILL_NOT -> WILL_NOT
  | `DO -> DO
  | `DO_NOT -> DO_NOT
  in
  let c = telnet_command_to_int b in
  let b = Cstruct.create 2 in
  Cstruct.set_uint8 b 0 0xFF ;
  Cstruct.set_uint8 b 1 c ;
  b

let emit_option cmd opt =
  let c = telnet_option_to_int opt in
  let cs = Cstruct.create 1 in
  Cstruct.set_uint8 cs 0 c ;
  Cstruct.concat [ emit_cmd cmd; cs ]

let handle_option state cmd what =
  let not_option = List.filter (fun (_, opt) -> opt <> what) state.client_options in
  let client_options, out = match cmd, client_option_state state what with
    | `WILL, Some `Requested -> (`Accepted, what) :: not_option, []
    | `WILL_NOT, Some `Requested -> (`Denied, what) :: not_option, []
    | `DO_NOT, Some `Requested -> (`Denied, what) :: not_option, []
    | `DO, _ -> state.client_options, [] (*`Option (`WILL_NOT, what)] XXX depends on state *)
    | `WILL, _ ->
      Printf.printf "requested, but won't %s\n%!" (telnet_option_to_string what) ;
      state.client_options, [`Option (`WILL_NOT, what)]
    | cmd, _ ->
      Printf.printf "ignoring unknown request %s %s\n%!" (cmd_to_string cmd) (telnet_option_to_string what) ;
      state.client_options, []
  in
  { state with machina = Data ; client_options }, out

let handle_sub _state = function
  | Negotiate_About_Window_Size -> EatSub (4, Negotiate_About_Window_Size, [])
  | _ -> Data

let of_list ints =
  let l = List.length ints in
  let res = Cstruct.create l in
  let rec go idx = function
    | x :: xs -> Cstruct.set_uint8 res idx x ; go (succ idx) xs
    | [] -> ()
  in
  go 0 ints ;
  res

let handle_subcommand _state cs = function
  | Negotiate_About_Window_Size ->
    let (width : int) = Cstruct.BE.get_uint16 cs 0
    and (height : int) = Cstruct.BE.get_uint16 cs 2
    in
    [ `Resize (width, height) ]
  | _ -> []

let handle_command state =
  let status machina = { state with machina } in
  function
  | IAC -> status Data, [`Data 0xFF]
  | SUBNEG_END -> status Data, []
  | NOP -> status Data, []
  | WILL -> status (Option `WILL), []
  | WILL_NOT -> status (Option `WILL_NOT), []
  | DO -> status (Option `DO), []
  | DO_NOT -> status (Option `DO_NOT), []
  | SUBNEG -> status Suboption, []
  | x ->
    Printf.printf "received %s\n%!" (telnet_command_to_string x) ;
    status Data, [] (* this is incorrect *)

let handle_main state data =
  match state.machina, data with
  | Data, 0xFF -> { state with machina = Command }, []
  | Data, c -> state, [`Data c]
  | Command, cmd -> (match int_to_telnet_command cmd with
      | None -> Printf.printf "unknown command %x\n%!" data ; { state with machina = Data }, []
      | Some x -> handle_command state x)
  | Option cmd, data -> (match int_to_telnet_option data with
      | None -> Printf.printf "unknown option %x\n%!" data ; { state with machina = Data }, []
      | Some x -> handle_option state cmd x)
  | Suboption, x -> (match int_to_telnet_option x with
      | None -> Printf.printf "unknown suboption %x\n%!" data ; { state with machina = Data }, []
      | Some x -> let machina = handle_sub state x in { state with machina }, [])
  | EatSub (1, opt, xs), x ->
    let out = handle_subcommand state (of_list (List.rev (x::xs))) opt in
    { state with machina = SubDone }, out
  | EatSub (x, opt, xs), c -> { state with machina = EatSub (pred x, opt, c :: xs) }, []
  | SubDone, 0xFF -> { state with machina = Command }, []
  | SubDone, x -> { state with machina = Data }, [`Data x]

let ev_s = function
  | `Data c -> Printf.printf "data %d:" (Cstruct.len c) ; Cstruct.hexdump c
  | `Resize (w, h) -> Printf.printf "resize %d, %d\n%!" w h

let handle state buf =
  (* Printf.printf "state in %s" (state_to_string state) ; *)
  let l = Cstruct.len buf in
  let rec go state idx acc =
    if idx >= l then
      state, List.flatten (List.rev acc)
    else
      let x = Cstruct.get_uint8 buf idx in
      let state, outs = handle_main state x in
      go state (succ idx) (outs :: acc)
  in
  let state, out = go state 0 [] in
  let maybe_data e = function
    | [] -> e
    | xs -> `Data (of_list (List.rev xs)) :: e
  in
  let data, options, events = List.fold_left ( fun (d, o, e) ev ->
      match ev with
      | `Option (cmd, x) -> ([], emit_option cmd x :: o, maybe_data e d)
      | `Data x -> (x :: d, o, e)
      | `Resize (w, h) -> ([], o, maybe_data (`Resize (w, h) :: e) d) )
      ([], [], []) out
  in
  let events = maybe_data events data in
  let options = Cstruct.concat (List.rev options) in
  (* Printf.printf "state out %s" (state_to_string state) ; *)
  (state, events, options)

let init () =
  let server_config = [
    `DO, Negotiate_About_Window_Size ;
    `DO, Binary_Transmission ;
    `WILL, Binary_Transmission ;
    (* `DO, Remote_Controlled_Trans_and_Echo ; *)
    (* `DO, Telnet_Suppress_Local_Echo ; *)
    `WILL, Echo ;
    (* `DO, Linemode ; *)
    (* `DO_NOT, Echo ; *)
    (*    `DO, Suppress_Go_Ahead ; *)
    (*    `WILL, Suppress_Go_Ahead ; *)
  ] in
  let client_options = List.map (fun (_, o) -> (`Requested, o)) server_config in
  ({ machina = Data ; server_config ; client_options },
   Cstruct.concat (List.map (fun (c, o) -> emit_option c o) server_config))

let encode cs =
  let l = Cstruct.len cs in
  let res = Cstruct.create (2 * l) in
  let off = ref 0 in
  for i = 0 to pred l do
    match Cstruct.get_uint8 cs i with
    | 0xFF ->
      Cstruct.BE.set_uint16 res (i + !off) 0xFFFF ;
      incr off
    | x -> Cstruct.set_uint8 res (i + !off) x
  done ;
  Cstruct.sub res 0 (l + !off)
