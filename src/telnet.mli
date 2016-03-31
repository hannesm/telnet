
module Server : sig
  type state

  val handle : state -> Cstruct.t ->
    (state * [ `Data of Cstruct.t | `Resize of int * int ] list * Cstruct.t)

  val init : unit -> (state * Cstruct.t)

  val encode : Cstruct.t -> Cstruct.t
end
