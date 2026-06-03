// <copyright file="ParseTree.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module ParseTree

/// <summary>
/// Represents an arithmetic expression.
/// </summary>
type Expr =
    | Num of int
    | Op of Operation

/// <summary>
/// Represents an arithmetic operation over two expressions.
/// </summary>
and Operation =
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr

/// <summary>
/// Evaluates an expression using continuation-passing style.
/// Returns Some result if evaluation succeeds, or None if evaluation fails.
/// </summary>
let evalTail expr =
    let rec eval expr continuation =
        let evalBinary left right operation =
            eval left (function
                | None ->
                    continuation None
                | Some leftValue ->
                    eval right (function
                        | None ->
                            continuation None
                        | Some rightValue ->
                            continuation (operation leftValue rightValue)))

        match expr with
        | Num n ->
            continuation (Some n)
        | Op (Add(left, right)) ->
            evalBinary left right (fun leftValue rightValue -> Some(leftValue + rightValue))
        | Op (Sub(left, right)) ->
            evalBinary left right (fun leftValue rightValue -> Some(leftValue - rightValue))
        | Op (Mul(left, right)) ->
            evalBinary left right (fun leftValue rightValue -> Some(leftValue * rightValue))
        | Op (Div(left, right)) ->
            evalBinary left right (fun leftValue rightValue ->
                if rightValue = 0 then
                    None
                else
                    Some(leftValue / rightValue))

    eval expr id

/// <summary>
/// Evaluates an expression using a simple recursive algorithm.
/// Returns Some result if evaluation succeeds, or None if evaluation fails.
/// </summary>
let rec eval expr =
    match expr with
    | Num n ->
        Some n
    | Op (Add(l, r)) ->
        match eval l, eval r with
        | Some a, Some b -> Some(a + b)
        | _ -> None
    | Op (Sub(l, r)) ->
        match eval l, eval r with
        | Some a, Some b -> Some(a - b)
        | _ -> None
    | Op (Mul(l, r)) ->
        match eval l, eval r with
        | Some a, Some b -> Some(a * b)
        | _ -> None
    | Op (Div(l, r)) ->
        match eval l, eval r with
        | Some a, Some b when b <> 0 -> Some(a / b)
        | _ -> None
