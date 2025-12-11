let () =
  (* NOTE: This reduces runtime by 50% by cutting the probability of gc latency spikes *)
  Gc.set { (Gc.get ()) with minor_heap_size = 128 * 1024 * 1024; space_overhead = 500 };
  let argv = Sys.argv in
  if Array.length argv == 1
  then failwith "missing command line arguments"
  else
    let module Soln = (val Advent.Registry.get_solution argv.(1)) in
    Soln.time_and_solve ()
;;
