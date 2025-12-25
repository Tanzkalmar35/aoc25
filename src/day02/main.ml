(* ========================================================= *)
(* Advent of Code 2025 - Day 2                               *)
(* Gift Shop                                                 *)
(* ========================================================= *)

(* ---------------- File Reader ---------------- *)

let read_lines filename =
  let chan = open_in filename in
  let rec loop acc =
    try
      let line = input_line chan in
      loop (line :: acc)
    with End_of_file ->
      close_in chan;
      List.rev acc
  in
  loop []

(* ---------------- Pattern Checks ---------------- *)

(* Part 1:
   String must be exactly two identical halves *)
let is_repeated_twice s =
  let len = String.length s in
  if len mod 2 <> 0 then
    false
  else
    let half = len / 2 in
    let left = String.sub s 0 half in
    let right = String.sub s half half in
    left = right

(* Part 2:
   String must be made of a block repeated at least twice *)
let is_repeated_at_least_twice s =
  let len = String.length s in
  let rec try_block k =
    if k > len / 2 then
      false
    else if len mod k <> 0 then
      try_block (k + 1)
    else
      let block = String.sub s 0 k in
      let rec check i =
        if i = len then true
        else if String.sub s i k = block then check (i + k)
        else false
      in
      if check k then true else try_block (k + 1)
  in
  try_block 1

(* ---------------- Range Processing ---------------- *)

let invalid_ids_in_range ~is_invalid ~start ~stop =
  let rec loop n acc =
    if n > stop then
      acc
    else
      let s = Int64.to_string n in
      if is_invalid s then
        loop (Int64.add n 1L) (n :: acc)
      else
        loop (Int64.add n 1L) acc
  in
  loop start []

let parse_line ~is_invalid line =
  Scanf.sscanf line "%Ld-%Ld" (fun start stop ->
      invalid_ids_in_range ~is_invalid ~start ~stop
    )

(* ---------------- Utilities ---------------- *)

let flatten lst =
  List.fold_left List.rev_append [] lst |> List.rev

let sum_int64 lst =
  List.fold_left Int64.add 0L lst

(* ---------------- Solvers ---------------- *)

let solve ~is_invalid lines =
  lines
  |> List.map (parse_line ~is_invalid)
  |> flatten
  |> sum_int64

(* ---------------- Main ---------------- *)

let () =
  let filename =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else "inputs/day02.txt"
  in

  let lines = read_lines filename in

  let part1 =
    solve ~is_invalid:is_repeated_twice lines
  in

  let part2 =
    solve ~is_invalid:is_repeated_at_least_twice lines
  in

  Printf.printf "[Part 1] Sum of invalid IDs: %Ld\n" part1;
  Printf.printf "[Part 2] Sum of invalid IDs: %Ld\n" part2
