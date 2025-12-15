open! Core

module type Solution = sig
  type t

  val label : string
  val input : string
  val parse_input : string -> t
  val solver : t -> string
end

module type T = sig
  val time_and_solve : unit -> unit
  val get_soln_string : unit -> string
end

module Make (M : Solution) : T = struct
  let current_timestamp_ms () =
    Time_ns.now ()
    |> Time_ns.to_int63_ns_since_epoch
    |> Int63.to_float
    |> fun ns -> ns /. 1_000_000.
  ;;

  let get_soln_string () = M.input |> Util.Io.read_file |> M.parse_input |> M.solver

  let time_and_solve () =
    print_endline M.label;
    let starttime = current_timestamp_ms () in
    get_soln_string () |> Printf.printf "Solution: %s\n";
    let endtime = current_timestamp_ms () in
    let timems = endtime -. starttime in
    timems
    |> fun ms ->
    let us = ms *. 1000. in
    if Float.(ms < 1.)
    then Printf.printf "Time: %f ms (%f μs)\n" ms us
    else Printf.printf "Time: %f ms\n" ms
  ;;
end
