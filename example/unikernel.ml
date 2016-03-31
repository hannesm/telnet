open Lwt.Infix

let red fmt    = Printf.sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = Printf.sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = Printf.sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt   = Printf.sprintf ("\027[36m"^^fmt^^"\027[m")

let (>>==) a fb = a >>= function
  | `Ok x -> fb x
  | _ -> Lwt.return_unit

module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct

  module T = Telnet_mirage.Make (S.TCPV4)
  module N = Notty_mirage.Term (T)

  let pps = Format.pp_print_string
  let ppi = Format.pp_print_int

  let pp_special fmt = function
    | `Escape       -> pps fmt "ESCAPE"
    | `Enter        -> pps fmt "ENTER"
    | `Tab          -> pps fmt "TAB"
    | `Backspace    -> pps fmt "BACKSPACE"
    | `Arrow `Up    -> pps fmt "UP"
    | `Arrow `Down  -> pps fmt "DOWN"
    | `Arrow `Left  -> pps fmt "LEFT"
    | `Arrow `Right -> pps fmt "RIGHT"
    | `Page `Up     -> pps fmt "PAGE UP"
    | `Page `Down   -> pps fmt "PAGE DOWN"
    | `Home         -> pps fmt "HOME"
    | `End          -> pps fmt "END"
    | `Insert       -> pps fmt "INSERT"
    | `Delete       -> pps fmt "DELETE"
    | `Function n   -> pps fmt "FN"; ppi fmt n

  let pp_mods fmt = function
    | [] -> ()
    | ms -> ms |> List.iter (fun m ->
        pps fmt @@ match m with `Meta -> "M" | `Ctrl -> "C" | `Shift -> "S"
      )

  let pp_mouse fmt = function
    | `Release -> pps fmt "Release"
    | `Drag    -> pps fmt "Drag"
    | `Move    -> pps fmt "Move"
    | `Press k ->
      pps fmt "Press ";
      pps fmt @@ match k with
      | `Left         -> "Left"
      | `Middle       -> "Middle"
      | `Right        -> "Right"
      | `Scroll `Up   -> "Scroll Up"
      | `Scroll `Down -> "Scroll Down"

  let rec take n xs =
    match n, xs with
    | 0, _ -> []
    | _, [] -> []
    | n, x::xs -> x :: take (pred n) xs

  let render term (w, h) xs =
    let open Notty in
    let magenta = A.(fg lightmagenta ++ bg black)
    and green   = A.(fg lightgreen   ++ bg black)
    and blue    = A.(fg lightblue    ++ bg black) in
    let pp_mods  = I.pp_attr green pp_mods
    and pp_mouse = I.pp_attr blue pp_mouse in
    let attr = magenta in
    let xs = take (h - 3) xs in
    let msg = I.string A.empty "Push keys."
    and ks = List.map (function
        | `Key (`Uchar u, mods) ->
          I.(uchar blue u 1 1 <|> strf ~attr " u%04x %a" u pp_mods mods)
        | `Key (#Unescape.key as k, mods) ->
          I.strf ~attr "%a %a" pp_special k pp_mods mods
        | `Mouse (e, (x, y), mods) ->
          I.strf ~attr "MOUSE %a (%d, %d) %a" pp_mouse e x y pp_mods mods
        | `Resize _ -> Printf.printf "resizing..." ; I.empty
      ) xs |> I.vcat in
    let w', h' = N.size term in
    I.(strf ~attr:A.(fg lightblack) "[ESC quits.] (%d, %d) (%d, %d)" w h w' h' <->
       vsnap ~align:`Top (h - 3) ks <-> void 0 1 <-> msg |> pad ~l:1 ~t:1)

  let handle c flow =
    N.create flow >>= function
    | `Ok term ->
      let rec loop (w, h) xs =
        let img = render term (w, h) xs in
        Notty_unix.output_image img ;
        N.write term (`Image img) >>== fun () ->
        N.read term >>= (function
            | `Ok (`Resize (w, h)) ->
              C.log_s c (green "resizing to %d, %d" w h) >>= fun () ->
              loop (w, h) xs
            | `Ok x ->
              Printf.printf "ok\n%!" ;
              loop (w, h) (x :: xs)
            | `Eof -> C.log_s c (red "eof while reading")
            | `Error e -> C.log_s c (red "error while reading %s" (N.error_message e)))
      in
      loop (80, 24) []
    | `Eof -> C.log_s c (red "eof while creating terminal")
    | `Error e ->
      C.log_s c (red "error creating terminal %s" (N.error_message e))

  let start c s =
    S.listen_tcpv4 s ~port:23 (fun flow ->
        let dst, dst_port = S.TCPV4.get_dest flow in
        C.log_s c (green "new tcp connection from %s %d" (Ipaddr.V4.to_string dst) dst_port)
        >>= fun () ->
        T.of_flow flow >>= (function
            | `Ok flow ->
              C.log_s c (green "handling flow now") >>= fun () ->
              handle c flow
            | `Eof -> C.log_s c (red "eof while of_flow")
          ));
    S.listen s

end

