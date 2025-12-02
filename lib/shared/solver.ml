type solution =
  { label : string
  ; inputfile : string
  ; solver : string -> string
  }

let cur_time_ms () = 1000. *. Sys.time ()

let solve solution =
  print_endline solution.label;
  let starttime = cur_time_ms () in
  solution.inputfile
  |> Util.Io.read_file
  |> solution.solver
  |> Printf.printf "Solution: %s\n";
  let endtime = cur_time_ms () in
  endtime -. starttime |> Printf.printf "Time: %f ms\n"
;;
