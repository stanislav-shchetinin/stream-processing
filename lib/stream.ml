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

let main_interpolation_process points interpolations step (last_interpolated : (t_algorithm * float option) list) : (t_algorithm * float option) list =
  let sorted_types = List.sort (fun a b -> compare a.wsize b.wsize) interpolations in
  
  List.map (fun interpolation ->
    let relevant_points = 
      if interpolation.wsize = 2 then
        List.rev points |> take 2 |> List.rev
      else
        take interpolation.wsize points
    in
    if List.length relevant_points >= interpolation.wsize then
      let end_point = List.hd (List.rev relevant_points) in
      let last_x = List.assoc_opt interpolation.algorithm last_interpolated in
      let start_x = 
        match last_x with
        | None -> (List.hd relevant_points).x
        | Some (Some x) -> x +. step
        | Some None -> (List.hd relevant_points).x
      in
      if start_x < end_point.x then
        let result =
          generate_x_values start_x end_point.x step
          |> Seq.map (fun x -> (x, interpolation.func relevant_points x))
          |> List.of_seq
        in
        print_endline (
          match interpolation.algorithm with 
          | Linear -> "Linear" 
          | Lagrange -> "Lagrange"
        );
        print_points result;
        (interpolation.algorithm, Some (fst (List.hd (List.rev result))))
      else
        (interpolation.algorithm, match last_x with Some x -> x | None -> None)
    else
      (interpolation.algorithm, None)
  ) sorted_types 

let rec continue_interpolation_process runner =
  let points = List.of_seq runner.points_stream in
  
  let last_interpolated = 
    if List.length points >= 2 then 
      main_interpolation_process points runner.algorithms runner.step runner.last_computed_points
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
