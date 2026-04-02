// <copyright file="Tests.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module Tests

open Xunit
open FsUnit.Xunit
open AsteriskSquare

[<Fact>]
let ``square 1 returns one star`` () =
    square 1 |> should equal "*"

[<Fact>]
let ``square 4 returns hollow square`` () =
    square 4 |> should equal "****\n*  *\n*  *\n****"

[<Fact>]
let ``square 0 returns empty string`` () =
    square 0 |> should equal ""