open! Shared.Util

module IngredientId = struct
  type t = int

  let t_of_string s = int_of_string s
end

type ingredients = IngredientId.t list

module IngredientIdRange = struct
  type t =
    { low : IngredientId.t
    ; high : IngredientId.t
    }

  (* low-high *)
  let t_of_string s =
    let segments = s |> String.split_on_char '-' in
    if List.length segments <> 2
    then failwith (Printf.sprintf "invalid range %s" s)
    else (
      let nth_to_id id = id |> List.nth segments |> IngredientId.t_of_string in
      { low = nth_to_id 0; high = nth_to_id 1 })
  ;;

  let compare t1 t2 =
    if t1.low = t2.low then compare t1.high t2.high else compare t1.low t2.low
  ;;

  let get_range_length t = t.high - t.low |> fun x -> x + 1
  let sort ranges = List.stable_sort compare ranges

  let merge ranges =
    if List.length ranges = 0 then failwith "invalid ranges list";
    let folder accumulator cur =
      let currange, allranges = accumulator in
      if cur.low <= currange.high + 1
      then { low = currange.low; high = max cur.high currange.high }, allranges
      else cur, currange :: allranges
    in
    let initial_state = List.nth ranges 0, [] in
    let cr, allranges = ranges |> List.fold_left folder initial_state in
    cr :: allranges
  ;;
end

type ranges = IngredientIdRange.t list
type u = ingredients * ranges

let u_of_input input : u =
  let ranges = ref [] in
  let ingredients = ref [] in
  input
  |> String.split_on_char '\n'
  |> ListUtil.remove_lines_shorter_than 1
  |> List.iter (fun line ->
    let numbers_count = line |> String.split_on_char '-' |> List.length in
    match numbers_count with
    | 1 -> ingredients := line :: !ingredients
    | 2 -> ranges := line :: !ranges
    | _ -> failwith (Printf.sprintf "invalid line %s" line));
  let ingredients_list = !ingredients |> List.map IngredientId.t_of_string in
  let ranges_list = !ranges |> List.map IngredientIdRange.t_of_string in
  ingredients_list, ranges_list
;;

type accumulator = IngredientIdRange.t * ranges

module IntervalAction = struct
  type t =
    | StartRange of IngredientId.t
    | EndRange of IngredientId.t
    | Ingredient of IngredientId.t

  let t_to_action_number = function
    | StartRange _ -> 0
    | Ingredient _ -> 1
    | EndRange _ -> 2
  ;;

  let t_to_coordinate = function
    | StartRange x -> x
    | Ingredient x -> x
    | EndRange x -> x
  ;;

  type u =
    { active : bool
    ; count : int
    }

  let create_t_list ingredients ranges =
    let open IngredientIdRange in
    let ingredient_actions = ingredients |> List.map (fun i -> Ingredient i) in
    let range_actions =
      ranges |> List.map (fun r -> [ StartRange r.low; EndRange r.high ]) |> List.flatten
    in
    ingredient_actions @ range_actions
  ;;

  let compare t1 t2 =
    let t1_coord, t2_coord = t_to_coordinate t1, t_to_coordinate t2 in
    let t1_action, t2_action = t_to_action_number t1, t_to_action_number t2 in
    if t1_coord = t2_coord then compare t1_action t2_action else compare t1_coord t2_coord
  ;;

  let sort_t_list ts = List.stable_sort compare ts

  let count_ingredients_within_intervals actions =
    let folder accumulator action =
      match action with
      | StartRange _ -> { active = true; count = accumulator.count }
      | Ingredient _ ->
        if not accumulator.active
        then accumulator
        else { active = accumulator.active; count = accumulator.count + 1 }
      | EndRange _ -> { active = false; count = accumulator.count }
    in
    actions |> List.fold_left folder { active = false; count = 0 } |> fun u -> u.count
  ;;
end
