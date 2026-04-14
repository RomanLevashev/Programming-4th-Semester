// <copyright file="LocalNetwork.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

namespace LocalNetwork

open System

/// <summary>
/// Represents an operating system installed on a computer.
/// </summary>
type OperatingSystem =
    | Windows
    | Linux
    | MacOS
    | Other of string

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

/// <summary>
/// Represents a computer in a local network.
/// </summary>
type Computer(id: int, operatingSystem: OperatingSystem, initiallyInfected: bool) =
    let mutable isInfected = initiallyInfected

    member _.Id = id
    member _.OperatingSystem = operatingSystem

    member _.IsInfected
        with get () = isInfected
        and private set value = isInfected <- value

    /// <summary>
    /// Attempts to infect this computer according to the given probability.
    /// </summary>
    member this.TryInfect(probability: float, randomSource: IRandomSource) =
        if this.IsInfected then
            false
        else
            let randomValue = randomSource.NextDouble()

            if randomValue < probability then
                this.IsInfected <- true
                true
            else
                false

/// <summary>
/// Stores infection probabilities for each operating system.
/// </summary>
type InfectionProbability(probabilitiesByOperatingSystem: Map<OperatingSystem, float>) =
    do
        probabilitiesByOperatingSystem
        |> Map.iter (fun _ probability ->
            if probability < 0.0 || probability > 1.0 then
                invalidArg
                    (nameof probabilitiesByOperatingSystem)
                    "Infection probabilities must be in [0; 1].")

    /// <summary>
    /// Returns infection probability for the given operating system.
    /// Returns 0.0 if probability is not configured.
    /// </summary>
    member _.GetProbability(os: OperatingSystem) =
        probabilitiesByOperatingSystem
        |> Map.tryFind os
        |> Option.defaultValue 0.0

/// <summary>
/// Represents a local network with computers and links between them.
/// </summary>
type Network(computers: Computer array, adjacencyMatrix: bool[,], infectionProbability: InfectionProbability) =
    do
        if isNull (box computers) then
            nullArg (nameof computers)

        if isNull (box adjacencyMatrix) then
            nullArg (nameof adjacencyMatrix)

        let size0 = adjacencyMatrix.GetLength(0)
        let size1 = adjacencyMatrix.GetLength(1)

        if size0 <> computers.Length || size1 <> computers.Length then
            invalidArg
                (nameof adjacencyMatrix)
                "Adjacency matrix size must match the number of computers."

    member _.Computers = computers

    /// <summary>
    /// Returns the current network infection state.
    /// </summary>
    member _.Snapshot() =
        computers |> Array.map (fun computer -> computer.IsInfected)

    member private _.HasInfectedNeighbor(index: int, infectedAtStepStart: bool array) =
        [| 0 .. computers.Length - 1 |]
        |> Array.exists (fun neighbor ->
            adjacencyMatrix[index, neighbor] && infectedAtStepStart[neighbor])

    /// <summary>
    /// Performs one discrete simulation step.
    /// Newly infected computers cannot infect others in the same step.
    /// Returns true if at least one computer became infected.
    /// </summary>
    member this.Step(randomSource: IRandomSource) =
        let infectedAtStepStart = this.Snapshot()
        let mutable changed = false

        for index in 0 .. computers.Length - 1 do
            let computer = computers[index]

            if not computer.IsInfected && this.HasInfectedNeighbor(index, infectedAtStepStart) then
                let probability = infectionProbability.GetProbability(computer.OperatingSystem)

                if computer.TryInfect(probability, randomSource) then
                    changed <- true

        changed

    /// <summary>
    /// Simulates infection and returns network states after each changed step.
    /// Stops when state no longer changes or max step count is reached.
    /// </summary>
    member this.SimulateUntilStable(randomSource: IRandomSource, ?maxSteps: int) =
        let limit = defaultArg maxSteps Int32.MaxValue
        let states = ResizeArray<bool array>()
        let mutable step = 0
        let mutable changed = true

        while changed && step < limit do
            changed <- this.Step(randomSource)

            if changed then
                states.Add(this.Snapshot())

            step <- step + 1

        states |> Seq.toList
