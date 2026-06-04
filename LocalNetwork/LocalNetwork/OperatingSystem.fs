// <copyright file="OperatingSystem.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

namespace LocalNetwork

/// <summary>
/// Represents an operating system installed on a computer.
/// </summary>
type IOperatingSystem =
    /// <summary>
    /// Operating system name.
    /// </summary>
    abstract member Name: string

    /// <summary>
    /// Probability of infection for this operating system.
    /// </summary>
    abstract member InfectionProbability: float

/// <summary>
/// Operating system with a name and infection probability.
/// </summary>
type OperatingSystem(name: string, probability: float) =
    do
        if probability < 0.0 || probability > 1.0 then
            invalidArg (nameof probability) "Infection probability must be in [0; 1]."

    interface IOperatingSystem with
        member _.Name = name
        member _.InfectionProbability = probability
