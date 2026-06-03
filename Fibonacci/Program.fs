// <copyright file="Program.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module Fibonacci

open System

/// <summary>
/// Calculates the n-th Fibonacci number.
/// </summary>
/// <param name="n">Index of the Fibonacci number.</param>
/// <returns>The n-th Fibonacci number.</returns>
let fibonacci (n: int) : bigint =
    if n < 0 then invalidArg "n" "n must be >= 0"

    let rec loop (i: int) (a: bigint) (b: bigint) : bigint =
        if i = n then a
        else loop (i + 1) b (a + b)

    loop 0 0I 1I

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
        printfn "%A" (fibonacci n)
        0
