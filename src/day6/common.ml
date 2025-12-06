open! Shared.Util

module NumberColumn = struct
  type t =
    | Add of int list
    | Multiply of int list

  let solve t =
    match t with
    | Add list -> list |> List.fold_left (fun acc cur -> acc + cur) 0
    | Multiply list -> list |> List.fold_left (fun acc cur -> acc * cur) 1
  ;;

  let t_of_raw_row row =
    let ints = ref [] in
    let op = ref ' ' in
    row
    |> List.iter (fun r ->
      let r0 = String.get r 0 in
      if r0 >= '0' && r0 <= '9' then ints := int_of_string r :: !ints else op := r0);
    match !op with
    | '+' -> Add !ints
    | '*' -> Multiply !ints
    | _ -> failwith "invalid op"
  ;;

  let t_list_of_raw_rows rows = ListUtil.uniform_zip rows |> List.map t_of_raw_row

  let op_of_cephalopod_row cr =
    let lastelem = List.nth cr (List.length cr - 1) in
    let lastchar = String.get lastelem (String.length lastelem - 1) in
    let allelems =
      cr
      |> List.map (fun s ->
        let ss = String.sub s 0 (String.length s - 1) |> String.trim in
        int_of_string ss)
    in
    match lastchar with
    | '+' -> Add allelems
    | '*' -> Multiply allelems
    | _ -> failwith "invalid op"
  ;;

  let t_list_of_cephalopod_rows rows = rows |> List.map op_of_cephalopod_row
end

module RawRow = struct
  type t = string list

  type accumulator =
    { prevchar : char
    ; curstr : string
    ; strings : t
    }

  let t_of_string str =
    str
    |> String.split_on_char ' '
    |> List.map String.trim
    |> List.filter_map (fun line -> if String.length line > 0 then Some line else None)
  ;;
end

module CephalopodRow = struct
  (* Operator always appears in the last row, at same position as first column of each problem *)
  (* Each inner (string list) is a list of the raw numbers including spaces. The outer list is all of them.*)
  type t = string list list

  type accumulator =
    { current_problem : string list
    ; all_problems : t
    }

  let is_all_space l = l |> List.fold_left (fun acc cur -> acc && cur = ' ') true
  let join col = col |> List.fold_left (fun acc cur -> acc ^ String.make 1 cur) ""

  let folder accumulator curcolumn =
    if is_all_space curcolumn
    then
      { current_problem = []
      ; all_problems = accumulator.current_problem :: accumulator.all_problems
      }
    else (
      let current_problem = join curcolumn :: accumulator.current_problem in
      { current_problem; all_problems = accumulator.all_problems })
  ;;

  let t_of_string_list strings =
    strings
    |> List.map StringUtil.charlist_of_string
    |> ListUtil.uniform_zip
    |> List.fold_left folder { current_problem = []; all_problems = [] }
    |> fun a -> a.current_problem :: a.all_problems
  ;;
end
