let () =
  let argv = Sys.argv in
  if Array.length argv == 1
  then failwith "missing command line arguments"
  else
    let module Soln = (val Advent.Registry.get_solution argv.(1)) in
    Soln.time_and_solve ()
;;
