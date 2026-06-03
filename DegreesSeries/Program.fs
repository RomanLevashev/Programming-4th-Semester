// <copyright file="Program.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module DegreesSeries

open System

/// <summary>
/// Builds a list of powers of two from 2^n to 2^(n + m).
/// Returns None if n or m is negative.
/// </summary>
/// <param name="n">Initial power of two.</param>
/// <param name="m">Number of following powers to include.</param>
/// <returns>
/// List of powers of two wrapped in Some, or None if arguments are invalid.
/// </returns>
let degreesSeries (n: int) (m: int) : bigint list option =
    if n < 0 || m < 0 then
        None
    else
        let first = 1I <<< n

        let rec loop i current lst =
            if i > m then
                List.rev lst
            else
                loop (i + 1) (current * 2I) (current :: lst)

        Some(loop 0 first [])

/// <summary>
/// Parses integer command-line arguments.
/// </summary>
/// <param name="args">Command-line arguments.</param>
/// <returns>
/// Parsed pair of integers, or None if the arguments are invalid.
/// </returns>
let tryParseArguments (args: string array) : (int * int) option =
    if args.Length <> 2 then
        None
    else
        match Int32.TryParse args[0], Int32.TryParse args[1] with
        | (true, n), (true, m) -> Some(n, m)
        | _ -> None

/// <summary>
/// Program entry point.
/// </summary>
/// <param name="args">Command-line arguments: n and m.</param>
/// <returns>Process exit code.</returns>
[<EntryPoint>]
let main (args: string array) =
    match tryParseArguments args with
    | None ->
        printfn "Expected two integer arguments: n m"
        1
    | Some(n, m) ->
        match degreesSeries n m with
        | None ->
            printfn "None"
            1
        | Some series ->
            printfn "%A" series
            0
