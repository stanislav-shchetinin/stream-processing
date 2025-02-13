open Lib

let () =
  Array.to_list Sys.argv |> List.tl |> Parser.parse_arguments |> Runner.initialize_runner
