// <copyright file="Program.fs" company="Roman Levashev">
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
/// Represents a command used by the tail-recursive tree mapping algorithm.
/// </summary>
type Cmd<'a, 'b> =
    | Visit of Tree<'a>
    | Build of 'b

/// <summary>
/// Maps a function over a tree using a tail-recursive algorithm.
/// Returns Some mappedTree if the transformation succeeds.
/// </summary>
let mapTreeTail f tree =
    let rec loop cmds acc =
        match cmds, acc with
        | [], [result] ->
            Some result

        | Visit Empty :: restCmds, _ ->
            loop restCmds (Empty :: acc)

        | Visit (Node(x, left, right)) :: restCmds, _ ->
            loop (Visit left :: Visit right :: Build (f x) :: restCmds) acc

        | Build y :: restCmds, rightMapped :: leftMapped :: restAcc ->
            loop restCmds (Node(y, leftMapped, rightMapped) :: restAcc)

        | _ ->
            None

    loop [Visit tree] []

/// <summary>
/// Maps a function over a tree using a simple recursive algorithm.
/// </summary>
let rec mapTree f tree =
    match tree with
    | Empty ->
        Empty
    | Node(x, left, right) ->
        Node(f x, mapTree f left, mapTree f right)