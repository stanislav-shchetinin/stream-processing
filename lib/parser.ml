open Interpolations
open Interpolation
open Stream

let parse args =
  let rec aux args stepa methods =
    match args with
    | [] -> {
        step = stepa; 
        points_stream = Seq.empty; 
        methods;
        last_computed_points = List.map (fun i -> (i.name, None)) methods;
      }
    | "-m" :: "linear" :: rest -> aux rest stepa (Linear.interpolation :: methods)
    | "-m" :: "lagrange" :: rest -> aux rest stepa (Lagrange.interpolation :: methods)
    | "-s" :: step_value :: rest -> aux rest (float_of_string step_value) methods
    | _ :: rest -> aux rest stepa methods
  in
  aux args 0.5 []
