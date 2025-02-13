open Interpolations.Types

let read_point () =
  let line = read_line () in
  match String.split_on_char ' ' line with
  | [xi; yi] -> 
    let x = float_of_string xi in
    let y = float_of_string yi in 
    {x; y}
  | _ -> failwith "Invalid point"
