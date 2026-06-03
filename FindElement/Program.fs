// <copyright file="Program.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module FindElement

/// <summary>
/// Finds the first index of an element in a list.
/// </summary>
/// <param name="x">Element to find.</param>
/// <param name="lst">List to search.</param>
/// <returns>
/// Index of the element wrapped in Some, or None if the element is not found.
/// </returns>
let findElement (x: int) (lst: int list) : int option =
    let rec loop i lst =
        match lst with
        | [] -> None
        | h :: t ->
            if h = x then Some i
            else loop (i + 1) t

    loop 0 lst

/// <summary>
/// Program entry point.
/// </summary>
/// <param name="_">Command-line arguments.</param>
/// <returns>Process exit code.</returns>
[<EntryPoint>]
let main _ =
    0
