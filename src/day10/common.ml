open! Shared.Util

module Bitmask = struct
  type t =
    { value : int
    ; bits : int list
    }

  let get_nth_bit t n = (t.value lsr n) land 1 = 1

  let t_of_bit_list bits =
    (* [1 ; 0 ; 1 ; 0 ; 1 ; 1] -> 0b101011 *)
    let value = bits |> List.fold_left (fun acc bit -> (acc lsl 1) lor bit) 0 in
    { value; bits }
  ;;

  let t_of_bit_indexes indexes =
    (* [1 ; 0 ; 1 ; 0 ; 1 ; 1] -> 0b101011 *)
    let bits_rev = Array.make 16 0 in
    let value =
      indexes
      |> List.fold_left
           (fun acc index ->
              bits_rev.(index) <- 1;
              acc lor (1 lsl index))
           0
    in
    { value; bits = bits_rev |> Array.to_list |> List.rev }
  ;;

  let t_of_int i =
    let value = i in
    let bits =
      Array.make 16 0
      |> Array.mapi (fun index _ -> ((1 lsl index) land i) lsr index)
      |> Array.to_list
      |> List.rev
    in
    { value; bits }
  ;;

  let to_string t =
    let v = t.value in
    let bits = t.bits |> List.map string_of_int |> String.concat "" in
    Printf.sprintf "Bitmask(value=%d, bits=%s)" v bits
  ;;
end

module ElfMachine = struct
  type t =
    { lights : Bitmask.t
    ; schematics : Bitmask.t list
    ; joltage : int array
    }

  let strip_string s =
    (* (apple) -> apple *)
    String.sub s 1 (String.length s - 2)
  ;;

  let lights_of_string s =
    strip_string s
    |> StringUtil.charlist_of_string
    |> List.rev
    |> List.map (function
      | '#' -> 1
      | '.' -> 0
      | _ -> failwith "invalid lights character")
    |> Bitmask.t_of_bit_list
  ;;

  let schematic_of_string s =
    strip_string s
    |> String.split_on_char ','
    |> List.map int_of_string
    |> Bitmask.t_of_bit_indexes
  ;;

  let joltage_of_string s =
    strip_string s |> String.split_on_char ',' |> List.map int_of_string |> Array.of_list
  ;;

  let t_of_string s =
    let segments = s |> String.split_on_char ' ' in
    let lights = ref None in
    let schematics = ref [] in
    let joltage = ref None in
    segments
    |> List.iter (fun segment ->
      let firstchar = String.get segment 0 in
      match firstchar with
      | '[' -> lights := Some (lights_of_string segment)
      | '(' -> schematics := schematic_of_string segment :: !schematics
      | '{' -> joltage := Some (joltage_of_string segment)
      | _ -> failwith "invalid start of sequence");
    let numschematics = List.length !schematics in
    match !lights, !joltage, numschematics with
    | _, _, 0 -> failwith "invalid schematics"
    | Some l, Some j, _ -> { lights = l; schematics = !schematics; joltage = j }
    | _ -> failwith "invalid input string"
  ;;

  let pows =
    Array.of_list [ 1; 2; 4; 8; 16; 32; 64; 128; 256; 512; 1024; 2048; 4096; 8192 ]
  ;;

  let get_result_for_bitmask_binary t bitmask =
    let open Bitmask in
    t.schematics
    |> List.fold_left
         (fun (xorresult, count, index) schematic ->
            if get_nth_bit bitmask index
            then schematic.value lxor xorresult, count + 1, index + 1
            else xorresult, count, index + 1)
         (0, 0, 0)
  ;;

  let get_minimum_solution_binary t =
    let open Bitmask in
    let numschematics = List.length t.schematics in
    let searchspace_bitmasks = ListUtil.range 0 (pows.(numschematics) - 1) in
    searchspace_bitmasks
    |> List.fold_left
         (fun minimum bm ->
            let bitmask = t_of_int bm in
            let result, count, _ = get_result_for_bitmask_binary t bitmask in
            if result = t.lights.value && count < minimum then count else minimum)
         64
  ;;
end
