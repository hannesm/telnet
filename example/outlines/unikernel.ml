open Notty
open Lwt.Infix

let red fmt    = Printf.sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = Printf.sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = Printf.sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt   = Printf.sprintf ("\027[36m"^^fmt^^"\027[m")

let grid xxs = xxs |> List.map I.hcat |> I.vcat

let outline attr i =
  let (w, h) = I.(width i, height i) in
  let chr x = I.uchar attr x 1 1
  and hbar  = I.uchar attr 0x2500 w 1
  and vbar  = I.uchar attr 0x2502 1 h in
  let (a, b, c, d) = (chr 0x256d, chr 0x256e, chr 0x256f, chr 0x2570) in
  grid [ [a; hbar; b]; [vbar; i; vbar]; [d; hbar; c] ]

module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct

  module T = Telnet_mirage.Make (S.TCPV4)
  module N = Notty_mirage.Term (T)

  open Notty

  let rec image = function
    | 0 -> I.string A.(fg lightwhite) "X"
    | n ->
        image (pred n) |>
        outline A.(fg (if n mod 2 = 0 then lightred else lightblack))

  let rec loop c term s =
    let img = image s |> I.hsnap 31 |> I.vsnap 31 in
    N.write term (`Image img) >>= function
      | `Ok () -> N.read term >>= fun _ -> loop c term (s + 1)
      | _      -> C.log_s c "shit."

  let start c s =
    S.listen_tcpv4 s ~port:23 (fun flow ->
        let dst, dst_port = S.TCPV4.get_dest flow in
        C.log_s c (green "new tcp connection from %s %d" (Ipaddr.V4.to_string dst) dst_port)
        >>= fun () ->
        T.of_flow flow >>= (function
            | `Ok flow ->
              C.log_s c (green "handling flow now") >>= fun () ->
                N.create flow >>= (function
                    `Ok term -> loop c term 1
                  | `Eof | `Error _ -> C.log_s c "init: shit.")
            | `Eof -> C.log_s c (red "eof while of_flow")
          ));
    S.listen s

end
