open Interpolations
open Interpolation
open Stream

let parse args =
  let rec aux args stepa interpolations =
    match args with
    | [] -> {
        step = stepa; 
        points_stream = Seq.empty; 
        interpolations;
        last_interpolated_x = List.map (fun i -> (i.name, None)) interpolations;
      }
    | "-m" :: "linear" :: rest -> aux rest stepa (Linear.interpolation :: interpolations)
    | "-m" :: "lagrange" :: rest -> aux rest stepa (Lagrange.interpolation :: interpolations)
    | "-s" :: step_value :: rest -> aux rest (float_of_string step_value) interpolations
    | _ :: rest -> aux rest stepa interpolations
  in
  aux args 0.5 []
