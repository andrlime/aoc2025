open Shared

type direction =
  | Left of int
  | Right of int

type accumulator =
  { offset : int
  ; numzeros : int
  }

let ( % ) x m =
  let r = x mod m in
  if r < 0 then r + m else r
;;

let initial_state = { offset = 50; numzeros = 0 }

let direction_of_string s =
  let len = String.length s in
  if len == 0
  then None
  else (
    let dir, rest = Util.StringUtil.slice s 1 in
    let offset = int_of_string rest in
    match dir with
    | "L" -> Some (Left offset)
    | "R" -> Some (Right offset)
    | _ -> failwith "invalid direction")
;;
