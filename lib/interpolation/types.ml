type t_algorithm = Linear | Lagrange

type t_point = {
  x: float;
  y: float;
}

type t_interpolation = {
  algorithm : t_algorithm;
  wsize : int;
  func : t_point list -> float -> float;
}

let algorithm_to_string = function
  | Linear -> "linear"
  | Lagrange -> "lagrange"

let create_interpolation algorithm wsize func = { algorithm; wsize; func } 
