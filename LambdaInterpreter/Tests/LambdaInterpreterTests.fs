// <copyright file="LambdaInterpreterTests.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module LambdaInterpreter.Tests

open FsUnit.Xunit
open Xunit
open global.LambdaInterpreter

let private normalize limit expr =
    match normalizeWithLimit limit expr with
    | NormalForm normal ->
        normal
    | LimitReached _ ->
        failwith "Expected normal form, but limit was reached"

[<Fact>]
let ``freeVars returns only unbound variables`` () =
    let expr =
        Abs("x", App(Var "x", App(Var "y", Var "z")))

    freeVars expr |> should equal (Set.ofList [ "y"; "z" ])

[<Fact>]
let ``substitute replaces a free variable`` () =
    substitute (App(Var "x", Var "z")) "x" (Var "y")
    |> should equal (App(Var "y", Var "z"))

[<Fact>]
let ``substitute does not rename binder when variable is absent from body`` () =
    substitute (Abs("y", Var "z")) "x" (Var "y")
    |> should equal (Abs("y", Var "z"))

[<Fact>]
let ``alpha conversion avoids capture`` () =
    let expr =
        App(Abs("x", Abs("y", App(Var "x", Var "y"))), Var "y")

    normalize 1000 expr |> toString |> should equal "\\y`.y y`"

[<Fact>]
let ``normalize identity application`` () =
    let expr =
        App(Abs("x", Var "x"), Var "y")

    normalize 1000 expr |> should equal (Var "y")

[<Fact>]
let ``normalize constant function`` () =
    let expr =
        App(App(Abs("x", Abs("y", Var "x")), Var "a"), Var "b")

    normalize 1000 expr |> should equal (Var "a")

[<Fact>]
let ``normalize higher order example`` () =
    let expr =
        App(App(Abs("f", Abs("x", App(Var "f", Var "x"))), Abs("z", Var "z")), Var "q")

    normalize 1000 expr |> should equal (Var "q")

[<Fact>]
let ``omega reaches limit`` () =
    let omega =
        App(Abs("x", App(Var "x", Var "x")), Abs("x", App(Var "x", Var "x")))

    match normalizeWithLimit 20 omega with
    | NormalForm _ ->
        failwith "Expected no normal form within the limit"
    | LimitReached lastExpr ->
        toString lastExpr |> should equal "(\\x.x x) (\\x.x x)"
