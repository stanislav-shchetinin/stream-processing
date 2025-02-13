open Printf
open Interpolations.Types

let print_points algorithm points =
  List.iter (fun { x; y } ->
    printf "> %s: %.1f %.1f\n" (algorithm_to_string algorithm) x y
  ) points
