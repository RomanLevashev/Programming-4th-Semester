// <copyright file="BinarySearchTree.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module BinarySearchTree

open System
open System.Collections
open System.Collections.Generic

type private Node<'a> =
    | Empty
    | Node of 'a * Node<'a> * Node<'a>

type private TreeEnumerator<'a>(root: Node<'a>) =
    let stack = Stack<'a * Node<'a>>()
    let mutable current = None

    let rec pushLeft node =
        match node with
        | Empty -> ()
        | Node(value, left, right) ->
            stack.Push(value, right)
            pushLeft left

    let getCurrent () =
        match current with
        | Some value -> value
        | None -> invalidOp "The enumerator is not positioned on an element."

    do pushLeft root

    interface IEnumerator<'a> with
        member _.Current = getCurrent ()

    interface IEnumerator with
        member _.Current = box (getCurrent ())

        member _.MoveNext() =
            if stack.Count = 0 then
                current <- None
                false
            else
                let value, right = stack.Pop()
                current <- Some value
                pushLeft right
                true

        member _.Reset() =
            stack.Clear()
            current <- None
            pushLeft root

    interface IDisposable with
        member _.Dispose() =
            stack.Clear()
            current <- None

/// <summary>
/// Immutable binary search tree of unique values, enumerated in ascending order.
/// </summary>
type BinarySearchTree<'a when 'a: comparison> private (root: Node<'a>) =
    /// <summary>Creates an empty tree.</summary>
    new() = BinarySearchTree(Empty)

    /// <summary>Returns a tree containing the value. Duplicates are ignored.</summary>
    member _.Add(value: 'a) =
        let rec insert node =
            match node with
            | Empty -> Node(value, Empty, Empty)
            | Node(item, left, right) ->
                let order = compare value item
                if order < 0 then Node(item, insert left, right)
                elif order > 0 then Node(item, left, insert right)
                else node

        BinarySearchTree(insert root)

    /// <summary>Checks whether the tree contains the value.</summary>
    member _.Contains(value: 'a) =
        let rec search node =
            match node with
            | Empty -> false
            | Node(item, left, right) ->
                let order = compare value item
                if order < 0 then search left
                elif order > 0 then search right
                else true

        search root

    /// <summary>Returns a tree without the value.</summary>
    member _.Remove(value: 'a) =
        let rec takeMin node =
            match node with
            | Empty -> invalidOp "Cannot take the minimum of an empty tree."
            | Node(item, Empty, right) -> item, right
            | Node(item, left, right) ->
                let minimum, remaining = takeMin left
                minimum, Node(item, remaining, right)

        let rec remove node =
            match node with
            | Empty -> Empty
            | Node(item, left, right) ->
                let order = compare value item
                if order < 0 then Node(item, remove left, right)
                elif order > 0 then Node(item, left, remove right)
                else
                    match left, right with
                    | Empty, _ -> right
                    | _, Empty -> left
                    | _ ->
                        let successor, remaining = takeMin right
                        Node(successor, left, remaining)

        BinarySearchTree(remove root)

    /// <summary>Creates an independent in-order enumerator.</summary>
    member _.GetEnumerator() : IEnumerator<'a> =
        new TreeEnumerator<'a>(root) :> IEnumerator<'a>

    interface IEnumerable<'a> with
        member this.GetEnumerator() = this.GetEnumerator()

    interface IEnumerable with
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator
