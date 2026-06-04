// <copyright file="Tests.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module Tests

open FsUnit.Xunit
open MapTree
open Xunit

[<Fact>]
let ``mapTreeTail maps empty tree`` () =
    Assert.Equal<Tree<int>>(Empty, mapTreeTail ((+) 1) Empty)

[<Fact>]
let ``mapTreeTail maps a non-empty tree`` () =
    let tree =
        Node(1, Node(2, Empty, Empty), Node(3, Empty, Node(4, Empty, Empty)))

    let expected =
        Node(2, Node(3, Empty, Empty), Node(4, Empty, Node(5, Empty, Empty)))

    mapTreeTail ((+) 1) tree |> should equal expected

[<Fact>]
let ``mapTreeTail matches simple recursive map`` () =
    let tree =
        Node(10, Node(20, Empty, Empty), Node(30, Empty, Empty))

    mapTreeTail ((*) 2) tree |> should equal (mapTree ((*) 2) tree)
