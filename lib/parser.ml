open Interpolations
open Types
open Stream

let parse args =
  let rec aux args stepa algorithms =
    match args with
    | [] -> {
        step = stepa; 
        points_stream = Seq.empty; 
        algorithms;
        last_computed_points = List.map (fun i -> (i.algorithm, None)) algorithms;
      }
    | "-a" :: "linear" :: rest -> aux rest stepa (Linear.interpolation :: algorithms)
    | "-a" :: "lagrange" :: rest -> aux rest stepa (Lagrange.interpolation :: algorithms)
    | "-s" :: step_value :: rest -> aux rest (float_of_string step_value) algorithms
    | _ :: rest -> aux rest stepa algorithms
  in
  aux args 0.5 []
