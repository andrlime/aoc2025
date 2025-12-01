open Shared

let solver input =
  input |> ignore;
  failwith "not implemented"
;;

let solution =
  { label = "Day 2, Part 1"; input = Util.read_file "./input/day1.txt"; solver }
;;
