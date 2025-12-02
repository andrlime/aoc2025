let () =
  let argv = Sys.argv in
  if Array.length argv == 1
  then failwith "missing command line arguments"
  else (
    let soln_id = Sys.argv.(1) in
    Shared.Solver.solve (Advent.Registry.get_solution soln_id))
;;
