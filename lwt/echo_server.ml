open Lwt.Infix

let string_of_unix_err err f p =
  Printf.sprintf "Unix_error (%s, %s, %s)"
    (Unix.error_message err) f p

let yap ~tag msg = Lwt_io.printf "[%s] %s\n%!" tag msg

let serve port callback =
  let tag = "server" in

  let server_s () =
    let open Lwt_unix in
    let s = socket PF_INET SOCK_STREAM 0 in
    setsockopt s SO_REUSEADDR true ;
    bind s (ADDR_INET (Unix.inet_addr_any, port)) ;
    listen s 10 ;
    s
  in

  let handle state fd addr =
    Lwt.async @@ fun () ->
      Lwt.catch (fun () -> callback state fd addr >>= fun () -> yap ~tag "<- handler done")
        (function
          | Unix.Unix_error (e, f, p) ->
            yap ~tag @@ "handler: " ^ (string_of_unix_err e f p)
          | exn -> yap ~tag ("handler: exception " ^ Printexc.to_string exn))
  in

  yap ~tag ("-> start @ " ^ string_of_int port) >>= fun () ->
  let rec loop s =
    Lwt.catch (fun () -> Lwt_unix.accept s >|= fun f -> `R f)
      (function
        | Unix.Unix_error (e, f, p) -> Lwt.return (`L (string_of_unix_err e f p))
        | exn -> Lwt.return (`L ("loop: exception " ^ Printexc.to_string exn))) >>= function
    | `R (fd, addr) ->
      yap ~tag "-> connect" >>= fun () ->
      let state, out = Telnet.Server.init () in
      Lwt_unix.write fd (Cstruct.to_string out) 0 (Cstruct.len out) >>= fun _w ->
      ( handle state fd addr ; loop s )
    | `L (msg) ->
      yap ~tag ("server socket: " ^ msg) >>= fun () -> loop s
    in
    loop (server_s ())

let lines fd =
  let buf = Bytes.create 256 in
  Lwt_unix.read fd buf 0 256 >>= function
  | 0 -> Lwt.fail (invalid_arg "end of connection")
  | l -> Lwt.return (Bytes.sub buf 0 l)

let echo_server port =
  serve port (fun state fd _addr ->
      let rec loop state =
        lines fd >>= fun str ->
        let state, events, options = Telnet.Server.handle state (Cstruct.of_string str) in
        Lwt_unix.write fd (Cstruct.to_string options) 0 (Cstruct.len options) >>= fun _w ->
        Lwt_list.iter_s (fun event ->
            match event with
            | `Resize (w,h) -> yap ~tag:"handler" (Printf.sprintf "size %d,%d" w h)
            | `Data s ->
              yap ~tag:"handler" ("+ " ^ (Cstruct.to_string s)) >>= fun () ->
              let out = Telnet.Server.encode s in
              Printf.printf "out:" ; Cstruct.hexdump out ;
              Lwt_unix.write fd (Cstruct.to_string out) 0 (Cstruct.len out) >>= fun _w ->
              Lwt.return_unit)
          events >>= fun () ->
        loop state
      in
      loop state)

let () =
  let port =
    try int_of_string Sys.argv.(1) with _ -> 4433
  in
  Lwt_main.run (echo_server port)
