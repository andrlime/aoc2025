open! Shared.Util

module Bank = struct
  type battery = int
  type bank = battery list

  let battery_of_char c = int_of_char c - int_of_char '0'

  let bank_of_string s =
    if String.length s = 0
    then None
    else Some (StringUtil.charlist_of_string s |> List.map battery_of_char)
  ;;
end

include Bank

type accumulator =
  { largestdigit : int
  ; largestindex : int
  }

let initial_state = { largestdigit = -1; largestindex = -1 }
