open Lib

let () =
  Array.to_list Sys.argv |> List.tl |> Parser.parse |> Stream.start_interpolation_process
