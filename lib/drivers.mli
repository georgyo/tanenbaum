(** Interfaces with core libraries to provide a command-line interface. *)
module Cli : sig
  (** Runs Advent of Code problem runner, via a command-line interface. *)
  val main : unit -> unit
end
