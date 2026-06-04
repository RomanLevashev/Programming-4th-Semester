// <copyright file="Network.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

namespace LocalNetwork

/// <summary>
/// Represents a local network with computers and links between them.
/// </summary>
type Network(computers: Computer array, adjacencyMatrix: bool[,]) =
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

    member private this.CanBeInfected(index: int, infectedAtStepStart: bool array) =
        let computer = computers[index]

        not computer.IsInfected
        && computer.OperatingSystem.InfectionProbability > 0.0
        && this.HasInfectedNeighbor(index, infectedAtStepStart)

    member private this.HasPotentialInfections() =
        let infected = this.Snapshot()

        [| 0 .. computers.Length - 1 |]
        |> Array.exists (fun index -> this.CanBeInfected(index, infected))

    /// <summary>
    /// Performs one discrete simulation step.
    /// Newly infected computers cannot infect others in the same step.
    /// Returns true if at least one computer became infected.
    /// </summary>
    member this.Step(random: IRandomSource) =
        let infectedAtStepStart = this.Snapshot()
        let mutable changed = false

        [| 0 .. computers.Length - 1 |]
        |> Array.filter (fun index -> this.CanBeInfected(index, infectedAtStepStart))
        |> Array.iter (fun index ->
            if computers[index].TryInfect(random) then
                changed <- true)

        changed

    /// <summary>
    /// Simulates infection and yields network state after each step.
    /// The sequence stops only when no infected computer can infect any vulnerable neighbor.
    /// </summary>
    member this.Simulate(random: IRandomSource) =
        seq {
            while this.HasPotentialInfections() do
                this.Step(random) |> ignore
                yield this.Snapshot()
        }
