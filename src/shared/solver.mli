module type Solution = sig
  type t

  val label : string
  val input : string
  val parse_input : string -> t
  val solver : t -> string
end

module type T = sig
  val time_and_solve : unit -> unit
  val get_soln_string : unit -> string
end

module Make : functor (_ : Solution) -> T
