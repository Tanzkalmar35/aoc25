type move =
  | Left of int
  | Right of int

(*Part 1*)

let calc_move_r start move =
    let after_mv = start + move in
    ((after_mv mod 100) + 100) mod 100

let calc_move_l start move =
    let after_mv = start - move in
    ((after_mv mod 100) + 100) mod 100

(*Part 2*)

let count_crossings_left starting_pos dist =
    let rec loop current_pos remaining_dist count =
        if remaining_dist = 0 then (current_pos, count)
        else
            let next_pos = calc_move_l current_pos 1 in
            let new_count = if next_pos = 0 then count + 1 else count in

            loop next_pos (remaining_dist - 1) new_count
    in
    loop starting_pos dist 0

let count_crossings_right starting_pos dist =
    let rec loop current_pos remaining_dist count =
        if remaining_dist = 0 then (current_pos, count)
        else
            let next_pos = calc_move_r current_pos 1 in
            let new_count = if next_pos = 0 then count + 1 else count in

            loop next_pos (remaining_dist - 1) new_count
    in
    loop starting_pos dist 0


(* The Parsing Function (Line -> Move) *)
let parse_line line =
  (* %c reads the L/R, %d reads the number *)
  Scanf.sscanf line "%c%d" (fun dir dist ->
    match dir with
    | 'L' -> Left dist
    | 'R' -> Right dist
    | _   -> failwith ("Unknown direction: " ^ String.make 1 dir)
  )

(* File Reader Helper *)
let read_lines filename =
  let chan = open_in filename in
  let rec read_loop acc =
    try
      let line = input_line chan in
      read_loop (line :: acc)
    with End_of_file ->
      close_in chan;
      List.rev acc (* Reverse because we prepended lines *)
  in
  read_loop []

(* The Solver Logic for part 1*)
let solve_part_1 lines =
  let start_pos = 50 in
  let start_count = 0 in

  let (final_pos, final_count) = lines
  |> List.map parse_line      (* Convert ["L60"; "R25"] -> [Left 60; Right 25] *)
  |> List.fold_left (fun (pos, count) move ->
          let new_pos = match move with
            (* Calculate new position based on move type *)
            | Left dist -> calc_move_l pos dist
            | Right dist -> calc_move_r pos dist
          in

          let new_count =
            if new_pos = 0 then count + 1
            else count
          in

          (new_pos, new_count)
     ) (start_pos, start_count)
  in

  (final_pos, final_count)

let solve_part_2 lines =
  let start_pos = 50 in
  let start_count = 0 in

  let (final_pos, final_count) = lines
  |> List.map parse_line      (* Convert ["L60"; "R25"] -> [Left 60; Right 25] *)
  |> List.fold_left (fun (pos, count) move ->
          let (new_pos, add) = match move with
            (* Calculate new position based on move type *)
            | Left dist -> count_crossings_left pos dist
            | Right dist -> count_crossings_right pos dist
          in

          let new_count = count + add in

          (new_pos, new_count)
     ) (start_pos, start_count)
  in

  (final_pos, final_count)

(* Execution Entry Point *)
let () =
  (* Get filename from command line, or default to standard path *)
  let filename =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else "inputs/day01.txt"
  in

  let lines = read_lines filename in

  (*Part 1*)
  let (final_pos, final_count) = solve_part_1 lines in
  Printf.printf "[Part 1] Final Position: %d\n" final_pos;
  Printf.printf "[Part 1] Final Result: %s\n" (string_of_int final_count);

  let (final_pos, final_count) = solve_part_2 lines in
  Printf.printf "[Part 2] Final Position: %d\n" final_pos;
  Printf.printf "[Part 2] Final Result: %s\n" (string_of_int final_count)
