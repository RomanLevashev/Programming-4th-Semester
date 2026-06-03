// <copyright file="Program.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module Factorial

open System

/// <summary>
/// Calculates the factorial of a non-negative integer.
/// </summary>
/// <param name="n">Number whose factorial should be calculated.</param>
/// <returns>Factorial of n.</returns>
let factorial (n: int) : bigint =
    let rec factorialTail (n: int) (acc: bigint) : bigint =
        if n < 0 then invalidArg "n" "n must be >= 0"
        elif n <= 1 then acc
        else factorialTail (n - 1) (acc * bigint n)

    factorialTail n 1I

/// <summary>
/// Parses one integer command-line argument.
/// </summary>
/// <param name="args">Command-line arguments.</param>
/// <returns>
/// Parsed integer wrapped in Some, or None if the arguments are invalid.
/// </returns>
let tryParseArgument (args: string array) : int option =
    if args.Length <> 1 then
        None
    else
        match Int32.TryParse args[0] with
        | true, n -> Some n
        | _ -> None

/// <summary>
/// Program entry point.
/// </summary>
/// <param name="args">Command-line arguments: n.</param>
/// <returns>Process exit code.</returns>
[<EntryPoint>]
let main (args: string array) =
    match tryParseArgument args with
    | None ->
        printfn "Expected one integer argument: n"
        1
    | Some n ->
        printfn "%A" (factorial n)
        0
