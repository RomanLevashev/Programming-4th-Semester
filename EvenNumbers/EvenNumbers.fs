// <copyright file="Program.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module EvenNumbers

/// <summary>
/// Counts even numbers using map and sum.
/// </summary>
/// <param name="xs">Input list.</param>
/// <returns>Number of even elements.</returns>
let mapCounter xs =
    xs
    |> List.map (fun x -> if x % 2 = 0 then 1 else 0)
    |> List.sum

/// <summary>
/// Counts even numbers using filter and length.
/// </summary>
/// <param name="xs">Input list.</param>
/// <returns>Number of even elements.</returns>
let filterCounter xs =
    xs
    |> List.filter (fun x -> x % 2 = 0)
    |> List.length

/// <summary>
/// Counts even numbers using fold.
/// </summary>
/// <param name="xs">Input list.</param>
/// <returns>Number of even elements.</returns>
let foldCounter xs =
    xs
    |> List.fold (fun acc x -> acc + (if x % 2 = 0 then 1 else 0)) 0

