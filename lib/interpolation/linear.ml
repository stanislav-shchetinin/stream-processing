let linear points x =
  match points with
  | [ (x0, y0); (x1, y1) ] ->
      if x0 = x1 then failwith "x0 must not be equal to x1"
      else ((x -. x0) *. (y1 -. y0) /. (x1 -. x0)) +. y0
  | _ -> failwith "Invalid set of points"

let interpolation = Interpolation.create_interpolation "Linear" 2 linear;
