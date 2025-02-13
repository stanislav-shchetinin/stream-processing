open Types

let lagrange points x =
  let basis xi points =
    List.fold_left (fun prod {x = xj; _} ->
      if xj = xi then prod else prod *. (x -. xj) /. (xi -. xj)
    ) 1. points
  in
  List.fold_left (fun acc {x = xi; y = yi} ->
    acc +. yi *. basis xi points
  ) 0. points

let interpolation = create_interpolation Lagrange 3 lagrange;
