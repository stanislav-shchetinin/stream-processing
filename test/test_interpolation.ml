open Alcotest
open Interpolations
open Types

let test_linear_interpolation () =
  let points = [ {x = 0.; y = 0.}; {x = 2.; y = 4.} ] in
  let x = 1. in
  let y = Linear.linear points x in
  check (float 0.01) "linear interpolation" 2. y

let test_lagrange_interpolation () =
  let points = [ {x = 0.; y = 0.}; {x = 1.; y = 1.}; {x = 2.; y = 4.} ] in
  let x = 1.5 in
  let y = Lagrange.lagrange points x in
  check (float 0.01) "lagrange interpolation" 2.25 y
