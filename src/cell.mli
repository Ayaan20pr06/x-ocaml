type t

val init : id:int -> Client.t -> Webcomponent.t -> t
val id : t -> int
val set_source : t -> string -> unit
val add_message : t -> int -> X_protocol.output list -> unit
val completed_run : t -> X_protocol.output list -> unit
val set_prev : prev:t option -> t -> unit
val receive_merlin : t -> Protocol.answer -> unit
