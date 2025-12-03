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

let sum l =
  let (+) a b =
    match (a,b) with
      | (None,x) | (x,None) -> x
      | (Some x,Some y)     -> Some (x+y)
  in
  let convert a = Some a in
  let opt_l=List.map convert l  in
  List.fold_left (+) None opt_l

let flatten lst =
  let rec aux lst acc = match lst with
    | [] -> acc
    | x :: xs -> aux xs (acc @ x)
  in aux lst []   

let parse_line line =
  Scanf.sscanf line "%d-%d" (fun start stop ->
      let rec iter_print num acc = 
          if num >= stop + 1 then 
              acc
          else begin          
              let s_num = string_of_int num in
              let len = String.length s_num in

              let new_acc = 
                  if len mod 2 = 0 then begin
                      let (first_half, second_half) = split_number s_num in 
                      
                      if first_half = second_half then begin
                          num :: acc
                      end else 
                          acc
                  end
                  else 
                      acc
              in
              
              iter_print (num + 1) new_acc
          end                 
      in
      iter_print start []
  )
    

(* The Solver Logic for part 1*)
let solve_part_1 lines =
    let l = lines
      |> List.map parse_line in

    sum (flatten l)

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
  let res = solve_part_1 lines in
  match res with
  | Some sum -> Printf.printf "[Part 1] Final sum: %d\n" sum;
  | None -> Printf.eprintf "[Part 1]: Error summing everything up \n";

  (* let (final_pos, final_count) = solve_part_2 lines in *)
  (* Printf.printf "[Part 2] Final Position: %d\n" final_pos; *)
  (* Printf.printf "[Part 2] Final Result: %s\n" (string_of_int final_count) *)

