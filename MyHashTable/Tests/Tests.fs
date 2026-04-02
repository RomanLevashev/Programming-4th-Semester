// <copyright file="Tests.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module MyHashTableTests

open Xunit
open FsUnit.Xunit
open MyHashTable

[<Fact>]
let ``Add makes element available`` () =
    let table = MyHashTable<int>(10, fun x -> x)

    table.Add 5

    table.Contains 5 |> should equal true

[<Fact>]
let ``Contains returns false for missing element`` () =
    let table = MyHashTable<int>(10, fun x -> x)

    table.Contains 42 |> should equal false

[<Fact>]
let ``Remove deletes existing element`` () =
    let table = MyHashTable<int>(10, fun x -> x)

    table.Add 7
    table.Remove 7

    table.Contains 7 |> should equal false

[<Fact>]
let ``Removing missing element does nothing`` () =
    let table = MyHashTable<int>(10, fun x -> x)

    table.Remove 100

    table.Contains 100 |> should equal false

[<Fact>]
let ``Add does not create duplicates`` () =
    let table = MyHashTable<int>(10, fun x -> x)

    table.Add 3
    table.Add 3
    table.Remove 3

    table.Contains 3 |> should equal false

[<Fact>]
let ``Handles collisions correctly`` () =
    let table = MyHashTable<int>(2, fun x -> x)

    table.Add 1
    table.Add 3
    table.Add 5

    table.Contains 1 |> should equal true
    table.Contains 3 |> should equal true
    table.Contains 5 |> should equal true

[<Fact>]
let ``Removing one collided element keeps others`` () =
    let table = MyHashTable<int>(2, fun x -> x)

    table.Add 1
    table.Add 3
    table.Add 5
    table.Remove 3

    table.Contains 1 |> should equal true
    table.Contains 3 |> should equal false
    table.Contains 5 |> should equal true