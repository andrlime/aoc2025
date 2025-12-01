module Util : sig
  val read_file : string -> string
  val split_string_into_lines : string -> string list
end

type solution =
  { label : string
  ; input : string
  ; solver : string -> string
  }

val solver : solution -> unit
