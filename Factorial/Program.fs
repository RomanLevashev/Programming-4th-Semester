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
/// Program entry point.
/// </summary>
/// <param name="_">Command-line arguments.</param>
/// <returns>Process exit code.</returns>
[<EntryPoint>]
let main _ =
    let n = Console.ReadLine() |> int
    printfn "%A" (factorial n)
    0
