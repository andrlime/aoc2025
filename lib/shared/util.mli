module Io : sig
  val read_file : string -> string
  val split_string_into_lines : string -> string list
end

module ArrayUtil : sig
  val iter2d : ('a -> unit) -> 'a array array -> unit
  val map2d : ('a -> 'b) -> 'a array array -> 'b array array
end

module StringUtil : sig
  val slice : string -> int -> string * string
  val slice_in_half : string -> string * string
  val charlist_of_string : string -> char list
end

module ListUtil : sig
  val remove_lines_shorter_than : int -> string list -> string list
  val zip : 'a list -> 'b list -> ('a * 'b) list
  val range : int -> int -> int list
  val all_elems_in_list_equal : 'a list -> bool
  val intmax : int list -> int
  val intsum : int list -> int
  val iter2d : ('a -> unit) -> 'a list list -> unit
  val map2d : ('a -> 'b) -> 'a list list -> 'b list list
end

module Neighbors : sig
  val neighbors_2d : (int * int) list
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
