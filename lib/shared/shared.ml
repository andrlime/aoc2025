module Util = struct
  let read_file path =
    let channel = open_in path in
    let len = in_channel_length channel in
    let content = really_input_string channel len in
    close_in channel;
    content
  ;;

  let split_string_into_lines str = str |> String.split_on_char '\n'
end

type solution =
  { label : string
  ; input : string
  ; solver : string -> string
  }

let solver solution =
  print_endline solution.label;
  let starttime = Sys.time () in
  let result = solution.solver solution.input in
  let endtime = Sys.time () in
  result |> Printf.printf "Solution: %s\n";
  (endtime -. starttime) *. 1000. |> Printf.printf "Time: %f ms\n"
;;
