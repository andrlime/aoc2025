open Advent.Registry

let get_solution_string key =
  let module Soln = (val get_solution key) in
  Soln.get_soln_string ()
;;
