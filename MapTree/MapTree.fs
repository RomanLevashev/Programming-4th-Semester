// <copyright file="MapTree.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module MapTree

/// <summary>
/// Represents a binary tree.
/// </summary>
type Tree<'a> =
    | Node of 'a * Tree<'a> * Tree<'a>
    | Empty

/// <summary>
/// Maps a function over a tree using continuation-passing style.
/// </summary>
let mapTreeTail f tree =
    let rec map tree continuation =
        match tree with
        | Empty ->
            continuation Empty
        | Node(x, left, right) ->
            map left (fun mappedLeft ->
                map right (fun mappedRight ->
                    continuation (Node(f x, mappedLeft, mappedRight))))

    map tree id

/// <summary>
/// Maps a function over a tree using a simple recursive algorithm.
/// </summary>
let rec mapTree f tree =
    match tree with
    | Empty ->
        Empty
    | Node(x, left, right) ->
        Node(f x, mapTree f left, mapTree f right)
