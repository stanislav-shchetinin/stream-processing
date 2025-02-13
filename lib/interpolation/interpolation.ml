type t_interpolation = {
  name : string;
  wsize : int;
  func : (float * float) list -> float -> float;
}

let create_interpolation name wsize func = { name; wsize; func } 
