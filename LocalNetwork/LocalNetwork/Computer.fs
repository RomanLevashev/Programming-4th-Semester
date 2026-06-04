// <copyright file="Computer.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

namespace LocalNetwork

/// <summary>
/// Represents a computer in a local network.
/// </summary>
type Computer(id: int, operatingSystem: IOperatingSystem, initiallyInfected: bool) =
    let mutable isInfected = initiallyInfected

    member _.Id = id
    member _.OperatingSystem = operatingSystem

    member _.IsInfected
        with get () = isInfected
        and private set value = isInfected <- value

    /// <summary>
    /// Attempts to infect this computer according to its operating system probability.
    /// </summary>
    member this.TryInfect(random: IRandomSource) =
        let wasInfected = this.IsInfected
        let randomValue = random.NextDouble()

        this.IsInfected <- wasInfected || randomValue < operatingSystem.InfectionProbability
        this.IsInfected <> wasInfected
