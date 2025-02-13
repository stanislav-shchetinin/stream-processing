open Printf
open Interpolations.Types

let print_points algorithm points =
  List.iter (fun { x; y } ->
    printf "> %s: %.1f %.1f\n" (algorithm_to_string algorithm) x y
  ) points

let read_point () =
  let line = read_line () in
  match String.split_on_char ' ' line with
  | [xi; yi] -> 
    let x = float_of_string xi in
    let y = float_of_string yi in 
    {x; y}
  | _ -> failwith "Invalid point"
