// <copyright file="Program.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

/// Functions for building and printing an asterisk square.
module AsteriskSquare

/// Returns a square of '*' characters as a string.
let square n =
    match n with
    | n when n <= 0 -> ""
    | 1 -> "*"
    | _ ->
        let topBottom = String.replicate n "*"
        let middle = "*" + String.replicate (n - 2) " " + "*"

        [topBottom] @ List.replicate (n - 2) middle @ [topBottom]
        |> String.concat "\n"

/// Prints the square to the console.
let printSquare n =
    square n |> printfn "%s"
