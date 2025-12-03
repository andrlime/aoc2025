type accumulator = { sum : int }

let initial_state = { sum = 0 }

module Range = struct
  type range =
    { lower : int
    ; upper : int
    }

  let range_of_string s =
    let split_s = s |> String.split_on_char '-' in
    if List.length split_s <> 2
    then None
    else (
      let lower = List.nth split_s 0 |> String.trim |> int_of_string in
      let upper = List.nth split_s 1 |> String.trim |> int_of_string in
      Some { lower; upper })
  ;;

  let sum_of_invalid_ids_in_range range is_invalid =
    let rec counter cur max count =
      if cur > max
      then count
      else if is_invalid cur
      then counter (cur + 1) max (count + cur)
      else counter (cur + 1) max count
    in
    counter range.lower range.upper 0
  ;;
end

include Range
