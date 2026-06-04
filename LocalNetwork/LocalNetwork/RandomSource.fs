// <copyright file="RandomSource.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

namespace LocalNetwork

open System

/// <summary>
/// Provides random values for infection simulation.
/// This abstraction allows deterministic unit testing via mock objects.
/// </summary>
type IRandomSource =
    abstract member NextDouble: unit -> float

/// <summary>
/// Default implementation of random value provider.
/// </summary>
type SystemRandomSource(?seed: int) =
    let random =
        match seed with
        | Some value -> Random(value)
        | None -> Random()

    interface IRandomSource with
        member _.NextDouble() = random.NextDouble()
