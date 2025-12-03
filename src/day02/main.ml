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

let split_number n =
  let len = String.length n in
  let mid = len / 2 in
  
  (* String.sub string start_index length *)
  let left_str = String.sub n 0 mid in
  let right_str = String.sub n mid mid in
  
  (int_of_string left_str, int_of_string right_str)

let parse_line line =
  Scanf.sscanf line "%d-%d" (fun start stop ->
      let rec iter_print num acc = 
          if num >= stop + 1 then 
              acc
          else begin          
              Printf.printf "%d," num;
              
              (* 1. Convert to string once so we can check length *)
              let s_num = string_of_int num in
              let len = String.length s_num in

              let new_acc = 
                  (* 2. CHECK: Only proceed if length is even *)
                  if len mod 2 = 0 then begin
                      (* It is safe to split now *)
                      (* Assuming split_number takes an int, pass 'num' back to it *)
                      (* If your split_number takes a string, pass 's_num' *)
                      let (first_half, second_half) = split_number s_num in 
                      
                      if first_half = second_half then
                          num :: acc
                      else 
                          acc
                  end
                  else 
                      (* Length is odd: skip logic, return accumulator unchanged *)
                      acc
              in
              
              iter_print (num + 1) new_acc
          end                 
      in
      let res = iter_print start [] in
      Printf.printf " (Invalid count: %d)\n" (List.length res)
  )
(* The Solver Logic for part 1*)
let solve_part_1 lines =
    lines
    |> List.iter parse_line

(* let solve_part_2 lines = *)
(*     lines List.iter *)

(* Execution Entry Point *)
let () =
  (* Get filename from command line, or default to standard path *)
  let filename = 
    if Array.length Sys.argv > 1 then Sys.argv.(1) 
    else "inputs/day02.txt" 
  in
  
  let lines = read_lines filename in
  
  (*Part 1*)
  solve_part_1 lines
  (**)
  (* let (final_pos, final_count) = solve_part_2 lines in *)
  (* Printf.printf "[Part 2] Final Position: %d\n" final_pos; *)
  (* Printf.printf "[Part 2] Final Result: %s\n" (string_of_int final_count) *)

