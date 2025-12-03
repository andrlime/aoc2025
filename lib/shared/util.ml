module Io = struct
  let read_file path =
    let channel = open_in path in
    let len = in_channel_length channel in
    let content = really_input_string channel len in
    close_in channel;
    content
  ;;

  let split_string_into_lines str = str |> String.split_on_char '\n'
end

module StringUtil = struct
  let slice str at =
    let firsthalf = String.sub str 0 at in
    let lasthalf = String.sub str at (String.length str - at) in
    firsthalf, lasthalf
  ;;

  let slice_in_half str =
    let halflength = String.length str / 2 in
    slice str halflength
  ;;

  let charlist_of_string str =
    let rec helper index accumulator =
      if index < 0
      then accumulator
      else (
        let curchar = String.get str index in
        helper (index - 1) (curchar :: accumulator))
    in
    helper (String.length str - 1) []
  ;;
end

module ListUtil = struct
  let rec zip l1 l2 =
    if List.length l1 <> List.length l2
    then failwith "cannot zip two lists of unequal length"
    else (
      match l1, l2 with
      | f1 :: r1, f2 :: r2 -> (f1, f2) :: zip r1 r2
      | _ -> [])
  ;;

  let range lower upper =
    let rec f cur lowerbound lst =
      if cur < lowerbound then lst else f (cur - 1) lowerbound (cur :: lst)
    in
    f upper lower []
  ;;

  let all_elems_in_list_equal list =
    let rec all_equal list thing =
      match list with
      | first :: rst -> first = thing && all_equal rst thing
      | [] -> true
    in
    if List.length list = 0 then true else all_equal list (List.nth list 0)
  ;;

  let rec intmax list =
    match list with
    | first :: [] -> first
    | first :: rest -> max first (intmax rest)
    | [] -> failwith "cannot take max of empty list"
  ;;

  let intsum list = list |> List.fold_left (fun acc cur -> acc + cur) 0
end

(* TODO: Maybe extend this to floats using a functor *)
module Physics = struct
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

  let dot q1 q2 =
    match q1, q2 with
    | Scalar x, Scalar y -> x * y
    | Vector2 { x = x1; y = y1 }, Vector2 { x = x2; y = y2 } -> (x1 * x2) + (y1 * y2)
    | Vector3 { x = x1; y = y1; z = z1 }, Vector3 { x = x2; y = y2; z = z2 } ->
      (x1 * x2) + (y1 * y2) + (z1 * z2)
    | VectorN list1, VectorN list2 ->
      ListUtil.zip list1 list2 |> List.fold_left (fun acc (x, y) -> acc + (x * y)) 0
    | _ -> failwith "cannot dot two incompatible types"
  ;;

  let distance_euclidean q1 q2 =
    match q1, q2 with
    | Scalar x, Scalar y -> Float.of_int (abs x - y)
    | Vector2 { x = x1; y = y1 }, Vector2 { x = x2; y = y2 } ->
      let dx = x1 - x2 in
      let dy = y1 - y2 in
      Float.sqrt ((Float.of_int dx ** 2.) +. (Float.of_int dy ** 2.))
    | Vector3 { x = x1; y = y1; z = z1 }, Vector3 { x = x2; y = y2; z = z2 } ->
      let dx = x1 - x2 in
      let dy = y1 - y2 in
      let dz = z1 - z2 in
      Float.sqrt
        ((Float.of_int dx ** 2.) +. (Float.of_int dy ** 2.) +. (Float.of_int dz ** 2.))
    | VectorN list1, VectorN list2 ->
      ListUtil.zip list1 list2
      |> List.fold_left (fun acc (x, y) -> acc +. (Float.of_int (x - y) ** 2.)) 0.0
      |> Float.sqrt
    | _ -> failwith "cannot compute euclidean distance between two incompatible types"
  ;;

  let distance_taxicab q1 q2 =
    match q1, q2 with
    | Scalar x, Scalar y -> abs x - y
    | Vector2 { x = x1; y = y1 }, Vector2 { x = x2; y = y2 } ->
      let dx = x1 - x2 in
      let dy = y1 - y2 in
      abs dx + abs dy
    | Vector3 { x = x1; y = y1; z = z1 }, Vector3 { x = x2; y = y2; z = z2 } ->
      let dx = x1 - x2 in
      let dy = y1 - y2 in
      let dz = z1 - z2 in
      abs dx + abs dy + abs dz
    | VectorN list1, VectorN list2 ->
      ListUtil.zip list1 list2 |> List.fold_left (fun acc (x, y) -> acc + abs (x - y)) 0
    | _ -> failwith "cannot compute euclidean distance between two incompatible types"
  ;;
end
