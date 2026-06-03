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
/// Program entry point.
/// </summary>
/// <param name="_">Command-line arguments.</param>
/// <returns>Process exit code.</returns>
[<EntryPoint>]
let main _ =
    let n = Console.ReadLine() |> int
    printfn "%A" (fibonacci n)
    0
