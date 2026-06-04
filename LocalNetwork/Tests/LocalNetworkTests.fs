// <copyright file="LocalNetworkTests.fs" company="Roman Levashev">
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

let private os name probability =
    OperatingSystem(name, probability) :> IOperatingSystem

let createChainNetwork probabilityForAll =
    let computers =
        [|
            Computer(0, os "Windows" probabilityForAll, true)
            Computer(1, os "Linux" probabilityForAll, false)
            Computer(2, os "MacOS" probabilityForAll, false)
        |]

    let links =
        array2D
            [ [ false; true; false ]
              [ true; false; true ]
              [ false; true; false ] ]

    Network(computers, links)

[<Fact>]
let ``infection with probability one spreads by layers`` () =
    let network = createChainNetwork 1.0
    let random = MockRandomSource([ 0.0; 0.0 ]) :> IRandomSource

    let states = network.Simulate(random) |> Seq.toList

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
    let random = MockRandomSource([]) :> IRandomSource

    let states = network.Simulate(random) |> Seq.toList

    states.Length |> should equal 0
    network.Snapshot() |> should equal [| true; false; false |]

[<Fact>]
let ``simulation does not stop after unlucky failed infection attempt`` () =
    let network = createChainNetwork 0.5
    let random = MockRandomSource([ 0.9; 0.0; 0.0 ]) :> IRandomSource

    let states = network.Simulate(random) |> Seq.take 3 |> Seq.toList

    states[0] |> should equal [| true; false; false |]
    states[1] |> should equal [| true; true; false |]
    states[2] |> should equal [| true; true; true |]

[<Fact>]
let ``disconnected computers are never infected`` () =
    let computers =
        [|
            Computer(0, os "Windows" 1.0, true)
            Computer(1, os "Linux" 1.0, false)
            Computer(2, os "MacOS" 1.0, false)
        |]

    let links =
        array2D
            [ [ false; true; false ]
              [ true; false; false ]
              [ false; false; false ] ]

    let network = Network(computers, links)
    let random = MockRandomSource([ 0.0 ]) :> IRandomSource

    let states = network.Simulate(random) |> Seq.toList

    states.Length |> should equal 1
    states[0] |> should equal [| true; true; false |]

[<Fact>]
let ``operating system validates infection probability`` () =
    (fun () -> OperatingSystem("BrokenOS", 1.5) |> ignore)
    |> should throw typeof<System.ArgumentException>
