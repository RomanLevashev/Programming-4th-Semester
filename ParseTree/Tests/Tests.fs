// <copyright file="Tests.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module Tests

open FsUnit.Xunit
open ParseTree
open Xunit

[<Fact>]
let ``evalTail evaluates arithmetic expression`` () =
    let expr =
        Op(Mul(Op(Add(Num 2, Num 3)), Op(Sub(Num 10, Num 4))))

    evalTail expr |> should equal (Some 30)

[<Fact>]
let ``evalTail returns None on division by zero`` () =
    let expr =
        Op(Div(Num 42, Op(Sub(Num 5, Num 5))))

    evalTail expr |> should equal None

[<Fact>]
let ``evalTail matches simple recursive evaluator`` () =
    let expressions =
        [ Num 1
          Op(Add(Num 2, Num 3))
          Op(Sub(Op(Mul(Num 4, Num 5)), Num 6))
          Op(Div(Op(Add(Num 20, Num 10)), Num 3))
          Op(Div(Num 1, Num 0)) ]

    expressions
    |> List.iter (fun expr -> evalTail expr |> should equal (eval expr))
