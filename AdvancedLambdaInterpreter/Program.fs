// <copyright file="Program.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module Program

open System
open AdvancedLambdaInterpreter

let private printResult (result: Result<string, string>) =
    match result with
    | Ok output ->
        if output <> String.Empty then
            Console.WriteLine output

        0
    | Error error ->
        Console.Error.WriteLine error
        1

[<EntryPoint>]
let main arguments =
    match arguments with
    | [| "--file"; path |] ->
        interpretFile path |> printResult
    | [| "--string"; input |] ->
        interpretString input |> printResult
    | _ ->
        Console.Error.WriteLine "Usage: AdvancedLambdaInterpreter --file <path> | --string <program>"
        1
