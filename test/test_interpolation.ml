open Alcotest
open Interpolations
open Types

let test_linear_interpolation () =
  let points = [ {x = 1.; y = 2.}; {x = 3.; y = 6.} ] in
  let x = 2. in
  let y = Linear.linear points x in
  check (float 0.01) "linear interpolation" 4. y

let test_lagrange_interpolation () =
  let points = [ {x = -1.; y = 1.}; {x = 0.; y = 0.}; {x = 2.; y = 4.} ] in
  let x = 1. in
  let y = Lagrange.lagrange points x in
  check (float 0.01) "lagrange interpolation" 1. y
