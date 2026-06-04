// <copyright file="LambdaInterpreter.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module LambdaInterpreter

/// <summary>
/// Lambda calculus expression.
/// </summary>
type Expr =
    /// <summary>Variable.</summary>
    | Var of string

    /// <summary>Abstraction.</summary>
    | Abs of string * Expr

    /// <summary>Application.</summary>
    | App of Expr * Expr

/// <summary>
/// Result of normalization with a step limit.
/// </summary>
type NormalizeResult =
    /// <summary>Normal form was reached.</summary>
    | NormalForm of Expr

    /// <summary>Step limit was reached before normalization finished.</summary>
    | LimitReached of Expr

/// <summary>
/// Returns the set of free variables.
/// </summary>
/// <param name="expr">Expression.</param>
/// <returns>Set of free variable names.</returns>
let rec freeVars expr =
    match expr with
    | Var x ->
        Set.singleton x
    | App(l, r) ->
        Set.union (freeVars l) (freeVars r)
    | Abs(x, body) ->
        Set.remove x (freeVars body)

let freshName baseName forbidden =
    let rec loop k =
        let candidate =
            if k = 0 then baseName + "`"
            else baseName + "`" + string k

        if Set.contains candidate forbidden then
            loop (k + 1)
        else
            candidate

    loop 0

/// <summary>
/// Performs capture-avoiding substitution.
/// </summary>
/// <param name="expr">Expression in which substitution is performed.</param>
/// <param name="varName">Variable to replace.</param>
/// <param name="replacement">Replacement expression.</param>
/// <returns>Expression after substitution.</returns>
let rec substitute expr varName replacement =
    match expr with
    | Var x when x = varName ->
        replacement
    | Var _ ->
        expr
    | App(l, r) ->
        App(substitute l varName replacement, substitute r varName replacement)
    | Abs(x, _) when x = varName ->
        expr
    | Abs(x, body) ->
        if not (Set.contains varName (freeVars body)) then
            expr
        elif Set.contains x (freeVars replacement) then
            let forbidden =
                Set.union (freeVars body) (freeVars replacement)
                |> Set.add varName

            let xFresh = freshName x forbidden
            let renamedBody = substitute body x (Var xFresh)
            Abs(xFresh, substitute renamedBody varName replacement)
        else
            Abs(x, substitute body varName replacement)

/// <summary>
/// Performs one normal-order beta-reduction step.
/// </summary>
/// <param name="expr">Expression to reduce.</param>
/// <returns>
/// Reduced expression, or <c>None</c> if no reduction is possible.
/// </returns>
let rec reduceOnceNormal expr =
    match expr with
    | App(Abs(x, body), arg) ->
        Some(substitute body x arg)

    | App(l, r) ->
        match reduceOnceNormal l with
        | Some l' -> Some(App(l', r))
        | None ->
            match reduceOnceNormal r with
            | Some r' -> Some(App(l, r'))
            | None -> None

    | Abs(x, body) ->
        match reduceOnceNormal body with
        | Some body' -> Some(Abs(x, body'))
        | None -> None

    | Var _ ->
        None

/// <summary>
/// Normalizes an expression using normal-order beta-reduction.
/// </summary>
/// <param name="limit">Maximum number of reduction steps.</param>
/// <param name="expr">Expression to normalize.</param>
/// <returns>
/// <c>NormalForm</c> if a normal form is reached; otherwise <c>LimitReached</c>.
/// </returns>
let normalizeWithLimit limit expr =
    let rec loop steps current =
        if steps <= 0 then
            LimitReached current
        else
            match reduceOnceNormal current with
            | Some next -> loop (steps - 1) next
            | None -> NormalForm current

    loop limit expr

/// <summary>
/// Converts an expression to text.
/// </summary>
/// <param name="expr">Expression to print.</param>
/// <returns>Textual representation of the expression.</returns>
let toString expr =
    let rec go prec e =
        match e with
        | Var x ->
            x
        | Abs(x, body) ->
            let s = "\\" + x + "." + go 0 body
            if prec > 0 then "(" + s + ")" else s
        | App(l, r) ->
            let s = go 1 l + " " + go 2 r
            if prec > 1 then "(" + s + ")" else s

    go 0 expr
