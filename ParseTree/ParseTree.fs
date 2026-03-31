// <copyright file="Program.fs" company="Roman Levashev">
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
/// Represents a command used by the tail-recursive expression evaluator.
/// </summary>
type Cmd =
    | Visit of Expr
    | DoAdd
    | DoSub
    | DoMul
    | DoDiv

/// <summary>
/// Evaluates an expression using a tail-recursive algorithm.
/// Returns Some result if evaluation succeeds, or None if evaluation fails.
/// </summary>
let evalTail expr =
    let rec loop cmds stack =
        match cmds, stack with
        | [], [result] ->
            Some result

        | Visit (Num n) :: restCmds, _ ->
            loop restCmds (n :: stack)

        | Visit (Op (Add (left, right))) :: restCmds, _ ->
            loop (Visit left :: Visit right :: DoAdd :: restCmds) stack

        | Visit (Op (Sub (left, right))) :: restCmds, _ ->
            loop (Visit left :: Visit right :: DoSub :: restCmds) stack

        | Visit (Op (Mul (left, right))) :: restCmds, _ ->
            loop (Visit left :: Visit right :: DoMul :: restCmds) stack

        | Visit (Op (Div (left, right))) :: restCmds, _ ->
            loop (Visit left :: Visit right :: DoDiv :: restCmds) stack

        | DoAdd :: restCmds, right :: left :: stackTail ->
            loop restCmds ((left + right) :: stackTail)

        | DoSub :: restCmds, right :: left :: stackTail ->
            loop restCmds ((left - right) :: stackTail)

        | DoMul :: restCmds, right :: left :: stackTail ->
            loop restCmds ((left * right) :: stackTail)

        | DoDiv :: restCmds, right :: left :: stackTail when right <> 0 ->
            loop restCmds ((left / right) :: stackTail)

        | DoDiv :: _, 0 :: _ ->
            None

        | _ ->
            None

    loop [Visit expr] []

/// <summary>
/// Evaluates an expression using a simple recursive algorithm.
/// Returns Some result if evaluation succeeds, or None if evaluation fails.
/// </summary>
let rec eval expr =
    match expr with
    | Num n ->
        Some n
    | Op (Add (l, r)) ->
        match eval l, eval r with
        | Some a, Some b -> Some (a + b)
        | _ -> None
    | Op (Sub (l, r)) ->
        match eval l, eval r with
        | Some a, Some b -> Some (a - b)
        | _ -> None
    | Op (Mul (l, r)) ->
        match eval l, eval r with
        | Some a, Some b -> Some (a * b)
        | _ -> None
    | Op (Div (l, r)) ->
        match eval l, eval r with
        | Some a, Some b when b <> 0 -> Some (a / b)
        | _ -> None