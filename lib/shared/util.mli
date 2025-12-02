module Io : sig
  val read_file : string -> string
  val split_string_into_lines : string -> string list
end

module StringUtil : sig
  val slice : string -> int -> (string * string)
end

module ListUtil : sig
  val zip : 'a list -> 'b list -> ('a * 'b) list
end

module Physics : sig
  type quantity =
    | Scalar of int
    | Vector2 of
        { x : int
        ; y : int
        }
    | Vector3 of
        { x : int
        ; y : int
        ; z : int
        }
    | VectorN of int list

  val dot : quantity -> quantity -> int
  val distance_euclidean : quantity -> quantity -> float
  val distance_taxicab : quantity -> quantity -> int
end
