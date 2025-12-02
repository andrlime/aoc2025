type solution =
  { label : string
  ; inputfile : string
  ; solver : string -> string
  }

val solve : solution -> unit
