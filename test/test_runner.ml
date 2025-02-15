open Alcotest
open Lib.Stream

let test_linspace () =
  let x_min = -2. in
  let x_max = 3. in
  let step = 1.5 in
  let result = linspace x_min x_max step |> List.of_seq in
  check (list (float 0.01)) "generate x values" [ -2.; -0.5; 1.; 2.5 ] result
