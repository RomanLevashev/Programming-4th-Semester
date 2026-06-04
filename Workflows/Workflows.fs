// <copyright file="Workflows.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module Workflows

open System

/// <summary>
/// Workflow builder that rounds intermediate and final floating-point values.
/// </summary>
type RoundingBuilder(precision: int) =
    do
        if precision < 0 || precision > 15 then
            invalidArg (nameof precision) "Rounding precision must be in [0; 15]."

    let round (value: float) =
        Math.Round(value, precision, MidpointRounding.AwayFromZero)

    /// <summary>
    /// Rounds the bound value and the continuation result.
    /// </summary>
    member _.Bind(value: float, continuation: float -> float) =
        continuation (round value) |> round

    /// <summary>
    /// Rounds the returned value.
    /// </summary>
    member _.Return(value: float) =
        round value

    /// <summary>
    /// Rounds a returned workflow value.
    /// </summary>
    member _.ReturnFrom(value: float) =
        round value

/// <summary>
/// Creates a workflow builder that rounds calculations to the given precision.
/// </summary>
let rounding precision =
    RoundingBuilder precision

/// <summary>
/// Workflow builder that parses string values as integers.
/// The workflow returns None if any bound string is not an integer.
/// </summary>
type StringCalculationBuilder() =
    /// <summary>
    /// Parses a string and passes the integer to the continuation.
    /// </summary>
    member _.Bind(value: string, continuation: int -> int option) =
        match Int32.TryParse value with
        | true, parsed -> continuation parsed
        | _ -> None

    /// <summary>
    /// Binds an existing optional integer value.
    /// </summary>
    member _.Bind(value: int option, continuation: int -> int option) =
        Option.bind continuation value

    /// <summary>
    /// Returns a successful calculation result.
    /// </summary>
    member _.Return(value: int) =
        Some value

    /// <summary>
    /// Returns an existing optional calculation result.
    /// </summary>
    member _.ReturnFrom(value: int option) =
        value

/// <summary>
/// Workflow that performs integer calculations with numbers represented as strings.
/// </summary>
let calculate =
    StringCalculationBuilder()
