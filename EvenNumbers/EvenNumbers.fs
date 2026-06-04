// <copyright file="EvenNumbers.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module EvenNumbers

let private isEven x =
    x % 2 = 0

/// <summary>
/// Counts even numbers using map and sum.
/// </summary>
/// <returns>Function that returns the number of even elements.</returns>
let countEvenByMap =
    List.map (fun x -> if isEven x then 1 else 0) >> List.sum

/// <summary>
/// Counts even numbers using filter and length.
/// </summary>
/// <returns>Function that returns the number of even elements.</returns>
let countEvenByFilter =
    List.filter isEven >> List.length

/// <summary>
/// Counts even numbers using fold.
/// </summary>
/// <returns>Function that returns the number of even elements.</returns>
let countEvenByFold =
    List.fold (fun acc x -> if isEven x then acc + 1 else acc) 0
