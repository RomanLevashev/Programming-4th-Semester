// <copyright file="Program.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module LambdaInterpreter

open System

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

let isIdentStart (c: char) = 
    Char.IsLetter(c) || c = '_'

let isIdentChar (c: char) = 
    Char.IsLetterOrDigit(c) || c = '_' || c = '\''

/// <summary>
/// Parses a lambda term.
/// </summary>
/// <param name="text">Input text.</param>
/// <returns>
/// Parsed expression, or <c>None</c> if parsing fails.
/// </returns>
let parse (text: string) : Expr option = 
    let n = text.Length
    
    let rec skipSpaces i = 
        if i < n && Char.IsWhiteSpace(text[i]) then
            skipSpaces (i + 1)
        else
            i

    let rec readIdentTail i = 
        if i < n && isIdentChar text[i] then
            readIdentTail(i + 1)
        else
            i

    let parseIdent i = 
        let i = skipSpaces i
        if i < n && isIdentStart text[i] then
            let j = readIdentTail (i + 1)
            Some(text.Substring(i, j - i), j)
        else
            None
    
    let rec buildAbs vars body = 
        match vars with
        | [] -> body
        | x :: xs -> buildAbs xs (Abs(x, body))

    let rec readParams acc i =
        let i = skipSpaces i 
        if i < n && isIdentStart text[i] then 
            match parseIdent i with 
            | Some (name, j) -> readParams (name :: acc) j
            | None -> None
        else
            Some(acc, i)

    let startsAtom i = 
        let i = skipSpaces i 
        i < n && (isIdentStart text[i] || text[i] = '(')

    let rec parseTerm i = 
        let i = skipSpaces i 
        if i < n && (text[i] = '\\' || text[i] = 'λ') then 
            parseAbs i 
        else
            parseApp i 
            
    and parseAbs i = 
        let i = skipSpaces i 
        if i < n && (text[i] = '\\' || text[i] = 'λ') then
            match readParams [] (i + 1) with
            | Some([], _) -> 
                None
            | Some(varsRev, j) ->
                let j = skipSpaces j 
                if j < n && text[j] = '.' then
                    match parseTerm (j + 1) with
                    | Some (body, k) -> Some(buildAbs varsRev body, k)
                    | None -> None
                else
                    None
            | None ->
                None
        else
            None
        
    and parseAtom i =
        let i = skipSpaces i 
        if i < n then
            if text[i] = '(' then
                match parseTerm (i + 1) with 
                | Some(expr, j) ->
                    let j = skipSpaces j
                    if j < n && text[j] = ')' then 
                        Some(expr, j + 1)
                    else 
                        None
                | None -> 
                    None
            else
                match parseIdent i with
                | Some (name, j) -> Some(Var name, j)
                | None -> None
        else
            None

    and parseApp i = 
        match parseAtom i with 
        | None -> None
        | Some(first, j) -> 
            let rec loop acc pos = 
                let pos = skipSpaces pos
                if startsAtom pos then
                    match parseAtom pos with
                    | Some (arg, nextPos) -> loop (App(acc, arg)) nextPos
                    | None -> None
                else
                    Some (acc, pos)

            loop first j

    match parseTerm 0 with
    | Some(expr, i) when skipSpaces i = n -> Some expr
    | _ -> None

/// <summary>
/// Returns the set of free variables.
/// </summary>
/// <param name="expr">Expression.</param>
/// <returns>Set of free variable names.</returns>
let rec freeVars expr = 
    match expr with
    | Var x -> 
        Set.singleton x 
    | App (l, r) -> 
        Set.union (freeVars l) (freeVars r)
    | Abs (x, body) ->
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

let rec renameBound oldName newName expr = 
    match expr with 
    | Var x ->
        if x = oldName then Var newName else expr
    | App(l, r) ->
        App(renameBound oldName newName l, renameBound oldName newName r)
    | Abs(x, body) ->
        if x = oldName then 
            Abs(x, body)
        else
            Abs(x, renameBound oldName newName body)

/// <summary>
/// Performs capture-avoiding substitution.
/// </summary>
/// <param name="expr">Expression in which substitution is performed.</param>
/// <param name="varName">Variable to replace.</param>
/// <param name="replacement">Replacement expression.</param>
/// <returns>Expression after substitution.</returns>
let rec substitute expr varName replacement =
    match expr with
    | Var x ->
        if x = varName then replacement else expr
    | App(l, r) ->
        App(substitute l varName replacement, substitute r varName replacement)
    | Abs (x, body) ->
        if x = varName then
            expr
        elif Set.contains x (freeVars replacement) then 
            let forbidden = 
                Set.union (freeVars body) (freeVars replacement)
                |> Set.add varName

            let xFresh = freshName x forbidden
            let renamedBody = renameBound x xFresh body
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
        | Some(l2) -> Some(App(l2, r))
        | None ->
            match reduceOnceNormal r with
            | Some(r2) -> Some(App(l, r2))
            | None -> None

    | Abs(x, body) ->
        match reduceOnceNormal body with
        | Some(body2) -> Some(Abs(x, body2))
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
let rec normalizeWithLimit limit expr = 
    let rec loop steps current = 
        if steps <= 0 then 
            LimitReached current
        else
            match reduceOnceNormal current with 
            | Some(next) -> loop (steps - 1) next
            | None -> NormalForm current

    loop limit expr

/// <summary>
/// Converts an expression to text.
/// </summary>
/// <param name="expr">Expression to print.</param>
/// <returns>Textual representation of the expression.</returns>
let rec toString expr = 
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

/// <summary>
/// Reads, parses, normalizes, and prints a lambda term.
/// </summary>
/// <param name="_">Command-line arguments.</param>
/// <returns>Process exit code.</returns>
[<EntryPoint>]
let main _ =
    let input = Console.ReadLine()

    match input with
    | null ->
        eprintfn "No term was entered."
        1
    | s when String.IsNullOrWhiteSpace s ->
        eprintfn "No term was entered."
        1
    | _ ->
        match parse input with
        | None ->
            eprintfn "Invalid lambda term format."
            2
        | Some expr ->
            match normalizeWithLimit 100000 expr with
            | NormalForm normal ->
                printfn "%s" (toString normal)
                0
            | LimitReached lastExpr ->
                eprintfn "Normal form was not found within the step limit."
                printfn "%s" (toString lastExpr)
                3