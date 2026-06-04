// <copyright file="WorkflowsTests.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module Workflows.Tests

open System
open FsUnit.Xunit
open Xunit
open Workflows

[<Fact>]
let ``rounding workflow returns sample result`` () =
    let result =
        rounding 3 {
            let! a = 2.0 / 12.0
            let! b = 3.5
            return a / b
        }

    result |> should equal 0.048

[<Fact>]
let ``rounding workflow rounds returned value`` () =
    let result =
        rounding 2 {
            return 1.235
        }

    result |> should equal 1.24

[<Fact>]
let ``rounding workflow validates precision`` () =
    (fun () -> rounding -1 |> ignore)
    |> should throw typeof<ArgumentException>

[<Fact>]
let ``calculate workflow returns sum for valid strings`` () =
    let result =
        calculate {
            let! x = "1"
            let! y = "2"
            let z = x + y
            return z
        }

    result |> should equal (Some 3)

[<Fact>]
let ``calculate workflow returns None for invalid strings`` () =
    let result =
        calculate {
            let! x = "1"
            let! y = "b"
            let z = x + y
            return z
        }

    result |> should equal None

[<Fact>]
let ``calculate workflow supports multiplication`` () =
    let result =
        calculate {
            let! x = "6"
            let! y = "7"
            return x * y
        }

    result |> should equal (Some 42)
