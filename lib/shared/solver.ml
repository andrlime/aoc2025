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
  let cur_time_ms () = 1000. *. Sys.time ()
  let get_soln_string () = M.input |> Util.Io.read_file |> M.parse_input |> M.solver

  let time_and_solve () =
    print_endline M.label;
    let starttime = cur_time_ms () in
    get_soln_string () |> Printf.printf "Solution: %s\n";
    let endtime = cur_time_ms () in
    endtime -. starttime |> Printf.printf "Time: %f ms\n"
  ;;
end
