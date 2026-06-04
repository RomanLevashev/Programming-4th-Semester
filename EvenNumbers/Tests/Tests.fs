// <copyright file="Tests.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module Tests

open EvenNumbers
open FsUnit.Xunit
open Xunit

let private counters =
    [ countEvenByMap; countEvenByFilter; countEvenByFold ]

[<Fact>]
let ``counters return zero for an empty list`` () =
    counters
    |> List.iter (fun countEven -> countEven [] |> should equal 0)

[<Fact>]
let ``counters return zero when there are no even numbers`` () =
    counters
    |> List.iter (fun countEven -> countEven [ 1; 3; 5; 7 ] |> should equal 0)

[<Fact>]
let ``counters count all even numbers`` () =
    counters
    |> List.iter (fun countEven -> countEven [ -4; -3; 0; 7; 10; 11 ] |> should equal 3)

[<Fact>]
let ``all implementations return the same count`` () =
    let xs = [ -10 .. 25 ]

    countEvenByMap xs |> should equal (countEvenByFilter xs)
    countEvenByFold xs |> should equal (countEvenByFilter xs)
