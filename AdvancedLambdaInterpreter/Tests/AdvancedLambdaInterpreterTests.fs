// <copyright file="AdvancedLambdaInterpreterTests.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module AdvancedLambdaInterpreterTests

open System.IO
open AdvancedLambdaInterpreter
open FsUnit.Xunit
open Xunit

let private interpret input =
    match interpretString input with
    | Ok result ->
        result
    | Error error ->
        failwith error

[<Fact>]
let ``parser handles multi-parameter abstraction`` () =
    match parseProgram "\\x y.x" with
    | Ok program ->
        Assert.Empty program.Definitions
        Assert.Equal(Abs("x", Abs("y", Var "x")), program.Expression)
    | Error error ->
        failwith error

[<Fact>]
let ``parser builds application as left associative`` () =
    match parseProgram "S K K" with
    | Ok program ->
        Assert.Empty program.Definitions
        Assert.Equal(App(App(Var "S", Var "K"), Var "K"), program.Expression)
    | Error error ->
        failwith error

[<Fact>]
let ``let keyword is not parsed as identifier`` () =
    match parseProgram "\\let.let" with
    | Ok _ ->
        failwith "Expected parser error."
    | Error _ ->
        ()

[<Fact>]
let ``interpreter reduces S K K example`` () =
    let input =
        """
        let S = \x y z.x z (y z)
        let K = \x y.x

        S K K
        """

    interpret input |> should equal "\\z.z"

[<Fact>]
let ``interpreter reads input from file`` () =
    let path = Path.GetTempFileName()

    try
        File.WriteAllText(path, "let I = \\x.x\n\n(\\x.x) I")

        match interpretFile path with
        | Ok result ->
            result |> should equal "\\x.x"
        | Error error ->
            failwith error
    finally
        File.Delete path

[<Fact>]
let ``bound variable shadows named definition`` () =
    let input =
        """
        let x = \z.z

        \x.x
        """

    interpret input |> should equal "\\x.x"

[<Fact>]
let ``cyclic definitions return error`` () =
    let input =
        """
        let A = B
        let B = A

        A
        """

    match interpretString input with
    | Ok result ->
        failwithf "Expected cyclic definition error, got %s." result
    | Error error ->
        Assert.Contains("Cyclic definition", error)
