open Lwt.Infix

module type SFLOW = sig

  type +'a io

  type input
  type output
  type flow
  type error

  val error_message : error -> string
  val read   : flow -> [`Ok of input | `Eof | `Error of error ] io
  val write  : flow -> output -> [`Ok of unit | `Eof | `Error of error ] io
  val writev : flow -> output list -> [`Ok of unit | `Eof | `Error of error ] io
  val close  : flow -> unit io

end

module type SFLOW_LWT = SFLOW with type 'a io = 'a Lwt.t

module type TERMINAL_LINK = SFLOW_LWT
  with type input  = [ `Data of Cstruct.t | `Resize of (int * int) ]
   and type output = [ `Data of Cstruct.t | `Line_edit of bool ]

let (>>==) a fb = a >>= function
  | `Ok x -> fb x
  | `Error _ | `Eof as e -> Lwt.return e

(* : TERMINAL_LINK *)
module Make (F : V1_LWT.FLOW) = struct
  type input  = [ `Data of Cstruct.t | `Resize of (int * int) ]
  type output = [ `Data of Cstruct.t | `Line_edit of bool ]

  module FLOW = F

  type +'a io = 'a Lwt.t

  type flow = {
    flow : F.flow ;
    mutable state : Server.state ;
    mutable linger : input list
  }

  type error = F.error

  let error_message = F.error_message

  let write_i flow buf =
    F.write flow buf >|= fun _w -> `Ok ()

  let write s = function
    | `Data buf ->
      let buf = Server.encode buf in
      write_i s.flow buf >|= fun _w -> `Ok ()
    | _ -> Lwt.return (`Ok ())

  let rec read s =
    match s.linger with
    | [] ->
      F.read s.flow >>== fun buffer ->
      let state, events, out = Server.handle s.state buffer in
      s.state <- state ;
      write_i s.flow out >>= fun _w ->
      (match events with
       | [] -> read s
       | x::xs -> s.linger <- xs ; Lwt.return (`Ok x))
    | hd::rest ->
      s.linger <- rest ;
      Lwt.return (`Ok hd)

  let writev s xs =
    Lwt_list.fold_left_s (fun r x ->
        match r with
        | `Ok () -> write s x
        | `Eof -> Lwt.return `Eof
        | `Error e -> Lwt.return (`Error e))
      (`Ok ())
      xs

  let close s = F.close s.flow

  let of_flow flow =
    let state, out = Server.init () in
    write_i flow out >>= function
    | `Ok () ->
      let flow = { flow ; state ; linger = [] } in
      Lwt.return (`Ok flow)
    | `Eof -> Lwt.return `Eof
end
