// <copyright file="Tests.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module LocalNetworkTests

open FsUnit.Xunit
open LocalNetwork
open Xunit

/// <summary>
/// Mock random source that returns predefined values in sequence.
/// </summary>
type MockRandomSource(values: float list) =
    let mutable remaining = values

    interface IRandomSource with
        member _.NextDouble() =
            match remaining with
            | value :: tail ->
                remaining <- tail
                value
            | [] ->
                failwith "MockRandomSource ran out of predefined values."

let createChainNetwork probabilityForAll =
    let computers =
        [|
            Computer(0, Windows, true)
            Computer(1, Linux, false)
            Computer(2, MacOS, false)
        |]

    let links =
        array2D
            [ [ false; true; false ]
              [ true; false; true ]
              [ false; true; false ] ]

    let probabilities =
        InfectionProbability(
            Map.ofList
                [ Windows, probabilityForAll
                  Linux, probabilityForAll
                  MacOS, probabilityForAll ]
        )

    Network(computers, links, probabilities)

[<Fact>]
let ``infection with probability one spreads by layers`` () =
    let network = createChainNetwork 1.0
    let random = MockRandomSource([ 0.0; 0.0; 0.0 ]) :> IRandomSource

    let states = network.SimulateUntilStable(random)

    states.Length |> should equal 2

    states[0] |> should equal [| true; true; false |]
    states[1] |> should equal [| true; true; true |]

[<Fact>]
let ``newly infected computer does not infect further in same step`` () =
    let network = createChainNetwork 1.0
    let random = MockRandomSource([ 0.0 ]) :> IRandomSource

    let changed = network.Step(random)

    changed |> should equal true
    network.Snapshot() |> should equal [| true; true; false |]

[<Fact>]
let ``infection with probability zero never spreads`` () =
    let network = createChainNetwork 0.0
    let random = MockRandomSource([ 0.0; 0.0; 0.0 ]) :> IRandomSource

    let states = network.SimulateUntilStable(random)

    states.Length |> should equal 0
    network.Snapshot() |> should equal [| true; false; false |]

[<Fact>]
let ``disconnected computers are never infected`` () =
    let computers =
        [|
            Computer(0, Windows, true)
            Computer(1, Linux, false)
            Computer(2, MacOS, false)
        |]

    let links =
        array2D
            [ [ false; true; false ]
              [ true; false; false ]
              [ false; false; false ] ]

    let probabilities =
        InfectionProbability(
            Map.ofList
                [ Windows, 1.0
                  Linux, 1.0
                  MacOS, 1.0 ]
        )

    let network = Network(computers, links, probabilities)
    let random = MockRandomSource([ 0.0; 0.0 ]) :> IRandomSource

    let states = network.SimulateUntilStable(random)

    states.Length |> should equal 1
    states[0] |> should equal [| true; true; false |]
