let solutions =
  Hashtbl.of_seq (List.to_seq [ "1a", Day1.Part1.solution; "1b", Day1.Part2.solution ])
;;

let get_solution key =
  try Hashtbl.find solutions key with
  | Not_found -> failwith (Printf.sprintf "solution %s not found" key)
;;
