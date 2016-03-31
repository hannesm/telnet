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

(* : TERMINAL_LINK *)
module Make (F : V1_LWT.FLOW) : sig
  module FLOW    : V1_LWT.FLOW

  include TERMINAL_LINK

  val of_flow : FLOW.flow -> [ `Ok of flow | `Eof ] Lwt.t
end
  with module FLOW = F
