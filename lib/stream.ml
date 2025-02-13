open Interpolations.Types
open Io

type interpolation_process = {
  step : float;
  points_stream : t_point Seq.t;
  algorithms : t_interpolation list;
  last_computed_points : (t_algorithm * float option) list;
}

let rec take n list =
  match list with
  | [] -> []
  | _ when n <= 0 -> []
  | x :: xs -> x :: take (n - 1) xs

let generate_x_values start_x end_x step =
  let rec aux current =
    if current >= end_x then Seq.Nil
    else 
      let next = current +. step in
      if next > end_x then Seq.Cons (current, fun () -> Seq.Nil)
      else Seq.Cons (current, fun () -> aux next)
  in
  fun () -> aux start_x

let main_interpolation_process points runner : (t_algorithm * float option) list =
  let sorted_methods = List.sort (fun a b -> compare a.wsize b.wsize) runner.algorithms in

  let get_points interpolation =
    if interpolation.wsize = 2 then take 2 (List.rev points) |> List.rev
    else take interpolation.wsize points
  in

  let get_start_x interpolation pts =
    match List.assoc_opt interpolation.algorithm runner.last_computed_points |> Option.join with
    | None -> (List.hd pts).x
    | Some x -> x +. runner.step
  in

  let interpolate interpolation x1 x2 pts =
    generate_x_values x1 x2 runner.step
    |> Seq.map (fun x -> (x, interpolation.func pts x))
    |> List.of_seq
  in

  let process_interpolation interpolation =
    let pts = get_points interpolation in
    if List.length pts < interpolation.wsize then
      (interpolation.algorithm, None)
    else
      let x_end = (List.hd (List.rev pts)).x in
      let x_start = get_start_x interpolation pts in

      if x_start < x_end then
        let result = interpolate interpolation x_start x_end pts in
        let last_x = Some (fst (List.hd (List.rev result))) in
        
        let method_name = match interpolation.algorithm with
          | Linear -> "Linear"
          | Lagrange -> "Lagrange"
        in
        print_endline method_name;
        print_points result;
        
        (interpolation.algorithm, last_x)
      else
        (interpolation.algorithm, List.assoc_opt interpolation.algorithm runner.last_computed_points |> Option.join)
  in

  List.map process_interpolation sorted_methods

let rec continue_interpolation_process runner =
let points = List.of_seq runner.points_stream in

let last_interpolated = 
  if List.length points >= 2 then 
    main_interpolation_process points runner
  else
    runner.last_computed_points
in

let new_point = read_point () in
let updated_points = Seq.append runner.points_stream (Seq.return new_point) in

let required_points = List.fold_left (fun acc interpolation -> max acc interpolation.wsize) 0 runner.algorithms in
let updated_points =
  if Seq.length updated_points > required_points then Seq.drop 1 updated_points else updated_points
in

continue_interpolation_process {
  runner with 
  points_stream = updated_points;
  last_computed_points = last_interpolated
}

let start_interpolation_process runner =
  let min_required_points = 2 in  

  let rec collect_initial_points remaining_points collected =
    if remaining_points = 0 then collected
    else 
      let new_point = read_point () in
      let updated_collected = Seq.append collected (Seq.return new_point) in
      collect_initial_points (remaining_points - 1) updated_collected
  in

  let initial_data_points = collect_initial_points min_required_points Seq.empty in
  
  let initial_interpolation_states = 
    List.map (fun method_info -> (method_info.algorithm, None)) runner.algorithms
  in

  continue_interpolation_process { 
    runner with
    points_stream = initial_data_points;
    last_computed_points = initial_interpolation_states 
  }
