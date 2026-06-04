// <copyright file="PointFree.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module PointFree

/// <summary>
/// Multiplies every list element by the given number.
/// </summary>
let multiplyEachElement multiplier values =
    List.map (fun value -> value * multiplier) values

/// <summary>
/// Same transformation with multiplication arguments reordered.
/// </summary>
let multiplyEachElementCommuted multiplier values =
    List.map (fun value -> multiplier * value) values

/// <summary>
/// Same transformation using partial application of multiplication.
/// </summary>
let multiplyEachElementPartiallyApplied multiplier values =
    List.map ((*) multiplier) values

/// <summary>
/// Same transformation with the list argument removed.
/// </summary>
let multiplyEachElementWithoutListArgument multiplier =
    List.map ((*) multiplier)

/// <summary>
/// Point-free form of multiplying every list element by the given number.
/// </summary>
let multiplyEachElementPointFree =
    List.map << (*)
