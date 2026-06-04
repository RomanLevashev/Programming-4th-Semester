// <copyright file="PointFreeTests.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module PointFree.Tests

open FsUnit.Xunit
open PointFree
open Xunit

[<Fact>]
let ``all forms multiply each element`` () =
    let multiplier = 3
    let values = [ -2; 0; 4; 7 ]
    let expected = [ -6; 0; 12; 21 ]

    multiplyEachElement multiplier values |> should equal expected
    multiplyEachElementCommuted multiplier values |> should equal expected
    multiplyEachElementPartiallyApplied multiplier values |> should equal expected
    multiplyEachElementWithoutListArgument multiplier values |> should equal expected
    multiplyEachElementPointFree multiplier values |> should equal expected

[<Fact>]
let ``point-free form matches original form`` () =
    let multiplier = -5
    let values = [ -3 .. 3 ]

    multiplyEachElementPointFree multiplier values
    |> should equal (multiplyEachElement multiplier values)
