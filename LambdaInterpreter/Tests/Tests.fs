// <copyright file="Program.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module Tests

open Xunit
open FsUnit.Xunit
open LambdaInterpreter

let parseOk text =
    match parse text with
    | Some expr -> expr
    | None -> failwith ("Parse failed for: " + text)

[<Fact>]
let ``parse variable`` () =
    let expr = parseOk "x"
    toString expr |> should equal "x"

[<Fact>]
let ``parse abstraction with one parameter`` () =
    let expr = parseOk "\\x.x"
    toString expr |> should equal "\\x.x"

[<Fact>]
let ``parse abstraction with several parameters`` () =
    let expr = parseOk "\\x y z.x"
    toString expr |> should equal "\\x.\\y.\\z.x"

[<Fact>]
let ``parse application`` () =
    let expr = parseOk "f x y"
    toString expr |> should equal "f x y"

[<Fact>]
let ``parse parentheses`` () =
    let expr = parseOk "f x y"
    toString expr |> should equal "(f x) y"

[<Fact>]
let ``parse invalid term returns None`` () =
    parse "\\x.\\y.\\z." |> should equal None

[<Fact>]
let ``normalize identity application`` () =
    let expr = parseOk "(\\x.x) y"

    match normalizeWithLimit 1000 expr with
    | NormalForm normal ->
        toString normal |> should equal "y"
    | LimitReached _ ->
        failwith "Expected normal form, but limit was reached"

[<Fact>]
let ``normalize constant function`` () =
    let expr = parseOk "(\\x.\\y.x) a b"

    match normalizeWithLimit 1000 expr with
    | NormalForm normal ->
        toString normal |> should equal "a"
    | LimitReached _ ->
        failwith "Expected normal form, but limit was reached"

[<Fact>]
let ``normalize higher order example`` () =
    let expr = parseOk "(\\f.\\x.f x) (\\z.z) q"

    match normalizeWithLimit 1000 expr with
    | NormalForm normal ->
        toString normal |> should equal "q"
    | LimitReached _ ->
        failwith "Expected normal form, but limit was reached"

[<Fact>]
let ``alpha conversion avoids capture`` () =
    let expr = parseOk "(\\x.\\y.x y) y"

    match normalizeWithLimit 1000 expr with
    | NormalForm normal ->
        toString normal |> should equal "\\y`.y y`"
    | LimitReached _ ->
        failwith "Expected normal form, but limit was reached"

[<Fact>]
let ``omega reaches limit`` () =
    let expr = parseOk "(\\x.x x) (\\x.x x)"

    match normalizeWithLimit 20 expr with
    | NormalForm _ ->
        failwith "Expected no normal form within the limit"
    | LimitReached lastExpr ->
        toString lastExpr |> should equal "(\\x.x x) (\\x.x x)"