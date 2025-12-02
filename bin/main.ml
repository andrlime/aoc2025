let () =
  let argv = Sys.argv in
  if Array.length argv == 1
  then failwith "missing command line arguments"
  else Advent.Registry.get_solution argv.(1) |> Shared.Solver.solve
;;
