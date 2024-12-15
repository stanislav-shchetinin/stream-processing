open Input

(* Функция для разбиения строки по разделителю *)
let split_string sep str =
  let rec aux acc i =
    try 
      let j = String.index_from str i sep in
      aux (String.sub str i (j - i) :: acc) (j + 1)
    with Not_found ->
      List.rev (String.sub str i (String.length str - i) :: acc)
  in
  aux [] 0

(* Рекурсивный парсер аргументов командной строки *)
let rec parse_args args =
  match args with
  | [] -> ([], None)
  | "-a" :: value :: rest ->
      let (algorithms, sampling_rate) = parse_args rest in
      (split_string ',' value @ algorithms, sampling_rate)
  | "-d" :: value :: rest ->
      let (algorithms, _) = parse_args rest in
      (algorithms, Some (int_of_string value))
  | arg :: _ -> failwith ("Неизвестный аргумент: " ^ arg)

(* Главная функция программы *)
let () =
  try
    let args = Array.to_list (Array.sub Sys.argv 1 (Array.length Sys.argv - 1)) in
    let (algorithms, sampling_rate) = parse_args args in
    match sampling_rate with
    | None -> Printf.printf "Частота дискретизации не указана.\n"
    | Some rate ->
        Printf.printf "Алгоритмы интерполяции: %s\n" (String.concat ", " (algorithms @ ["linear"]));
        Printf.printf "Частота дискретизации: %d\n" rate;
        let point = input_point () in
        print_point point
  with
  | Failure msg -> Printf.eprintf "Ошибка: %s\n" msg
  | _ -> Printf.eprintf "Произошла неизвестная ошибка.\n"
