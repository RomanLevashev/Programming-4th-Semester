// <copyright file="Program.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module ListReverse

/// <summary>
/// Reverses a list using a tail-recursive algorithm.
/// </summary>
/// <param name="lst">List to reverse.</param>
/// <returns>Reversed list.</returns>
let listReverse (lst: 'a list) : 'a list =
    let rec loop (current: 'a list) (acc: 'a list) : 'a list =
        match current with
        | [] -> acc
        | h :: t -> loop t (h :: acc)

    loop lst []

/// <summary>
/// Program entry point.
/// </summary>
/// <param name="_">Command-line arguments.</param>
/// <returns>Process exit code.</returns>
[<EntryPoint>]
let main _ =
    0
