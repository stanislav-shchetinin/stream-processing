open Printf
open Interpolations.Types

let print_points points =
  let x_values = List.map fst points in
  let y_values = List.map snd points in
  List.iter (printf "%.2f\t") x_values;
  print_newline ();
  List.iter (printf "%.2f\t") y_values;
  print_newline ()

let read_point () =
  let line = read_line () in
  match String.split_on_char ' ' line with
  | [xi; yi] -> 
    let x = float_of_string xi in
    let y = float_of_string yi in 
    {x; y}
  | _ -> failwith "Invalid point"
