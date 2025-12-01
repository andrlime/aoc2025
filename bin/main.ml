let () =
  let soln_id = Sys.argv.(1) in
  Shared.solver (Advent.Registry.get_solution soln_id)
;;
