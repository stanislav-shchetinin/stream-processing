(* Тип для представления точки *)
type point = { x : float; y : float }

(* Исключение для ошибок ввода *)
exception InvalidInput of string

(* Функция для чтения строки и преобразования в точку *)
let input_point () =
  print_endline "Введите точку в формате 'X Y':";
  try
    let line = read_line () in
    match String.split_on_char ' ' line |> List.filter (fun s -> String.length s > 0) with
    | [x_str; y_str] ->
        let x = float_of_string x_str in
        let y = float_of_string y_str in
        { x; y }
    | _ -> raise (InvalidInput "Ошибка: точка должна быть записана в формате 'X Y'.")
  with Failure _ -> raise (InvalidInput "Ошибка: X и Y должны быть числами.")

(* Функция для вывода точки *)
let print_point point =
  Printf.printf "Точка: X = %.2f, Y = %.2f\n" point.x point.y
