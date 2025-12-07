open! Shared.Util

module GridTile = struct
  type t =
    | Empty
    | Beam
    | Splitter

  let t_list_of_line str =
    str
    |> StringUtil.charlist_of_string
    |> List.map (function
      | '.' -> Empty
      | 'S' | '|' -> Beam
      | '^' -> Splitter
      | _ -> failwith "invalid character")
  ;;

  let print_row list =
    list
    |> List.iter (fun t ->
      match t with
      | Empty -> print_char '.'
      | Beam -> print_char '|'
      | Splitter -> print_char '^');
    print_endline ""
  ;;

  let add_beam array index =
    if index < 0 || index >= Array.length array
    then ()
    else (
      let curval = Array.get array index in
      let newval =
        match curval with
        | Beam -> Beam
        | Splitter -> failwith "pretty sure this is impossible"
        | Empty -> Beam
      in
      Array.set array index newval)
  ;;

  let set_array_for_characters array index (curchar, nextchar) =
    (* if nextchar is a splitter, set beams
    if nextchar is empty, then prop whatever curchar is
    nextchar is by invariant not a beam *)
    let splitcount = ref 0 in
    let _ =
      match nextchar with
      | Empty ->
        let is_not_occupied = Array.get array index <> Beam in
        if is_not_occupied then Array.set array index curchar
      | Splitter ->
        if curchar = Beam
        then (
          add_beam array (index - 1);
          add_beam array (index + 1);
          splitcount := !splitcount + 1)
      | Beam -> failwith "next row cannot have a beam"
    in
    !splitcount
  ;;

  let create_new_next_row cur next =
    if List.length cur <> List.length next then failwith "rows not equal length";
    let arr = Array.make (List.length cur) Empty in
    let pairs = ListUtil.zip cur next in
    let splitcount = ref 0 in
    List.iteri
      (fun index pair ->
         let dsplitcount = set_array_for_characters arr index pair in
         splitcount := !splitcount + dsplitcount)
      pairs;
    Array.to_list arr, !splitcount
  ;;
end

module TachyonState = struct
  type t =
    { current_row : GridTile.t list
    ; split_count : int
    }

  let folder accumulator next_row =
    let new_next_row, d_split_count =
      GridTile.create_new_next_row accumulator.current_row next_row
    in
    (* Uncomment to print a cool tree: *)
    (* GridTile.print_row new_next_row; *)
    { current_row = new_next_row; split_count = accumulator.split_count + d_split_count }
  ;;
end

module Parser = struct
  type t = GridTile.t list list
  type u = GridTile.t array array

  let t_of_line_list = List.map GridTile.t_list_of_line

  let u_of_line_list list =
    list |> t_of_line_list |> List.rev |> List.map Array.of_list |> Array.of_list
  ;;
end

module TachyonDpSolver = struct
  type dpstate = int
  type table = dpstate array array

  let create_dp_table grid =
    let rows = Array.length grid in
    let cols = Array.length grid.(0) in
    let a = Array.make_matrix rows cols 0 in
    a.(0) <- Array.make cols 1;
    a
  ;;

  let retrieve_s dp grid =
    let open GridTile in
    let row_num = Array.length grid - 1 in
    let row_with_s = grid.(row_num) in
    let col_num = ref 0 in
    row_with_s
    |> Array.iteri (fun i c ->
      match c with
      | Beam -> col_num := i
      | _ -> ());
    dp.(row_num).(!col_num)
  ;;

  let solve_dp (u : Parser.u) : dpstate =
    (* Note that as we are solving bottom up, we flip the grid and dp table for convenience *)
    (* Use dp[index + 1] and grid[index] to create dp[index] *)
    (* Invariant: there is a blank row between every row of input *)
    (* Invariant: there is padding around the sides *)
    (* RR: if prevrow[j] = Splitter then sum(dpprevrow(j-1), dpprevrow(j+1)) *)
    (*     if prevrow[j] = Empty then fail *)
    (*     if prevrow[j] = some number then that number *)
    (* Return: value at S *)
    (* NOTE: This doesn't do any input sanity checking but it is mathematically correct *)
    let dp = create_dp_table u in
    ListUtil.range 1 (Array.length u - 1)
    |> List.iter (fun currow ->
      let open GridTile in
      let prevrow_dp = dp.(currow - 1) in
      let prevrow_grid = u.(currow - 1) in
      let currow_grid = u.(currow) in
      let new_currow =
        currow_grid
        |> Array.mapi (fun index tile ->
          match tile, prevrow_grid.(index) with
          | Splitter, _ -> 0
          | Beam, _ | Empty, Empty -> prevrow_dp.(index)
          | Empty, Splitter -> prevrow_dp.(index - 1) + prevrow_dp.(index + 1)
          | Empty, Beam -> failwith "impossible case")
      in
      dp.(currow) <- new_currow);
    retrieve_s dp u
  ;;
end
