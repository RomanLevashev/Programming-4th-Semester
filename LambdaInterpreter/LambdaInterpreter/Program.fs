// <copyright file="Program.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module LambdaInterpreter

open System


/// Represents a lambda calculus expression.
///
/// Supported expression forms:
/// - variable: x
/// - abstraction: \x.body
/// - application: f x
///
/// The internal representation uses:
/// - Var(name) for a variable,
/// - Abs(parameter, body) for an abstraction,
/// - App(left, right) for an application.
type Expr = 
    | Var of string
    | Abs of string * Expr
    | App of Expr * Expr

/// Represents the result of normalization with a step limit.
///
/// Cases:
/// - NormalForm(expr): a normal form was reached within the limit;
/// - LimitReached(expr): the limit was reached before a normal form was found,
///   and expr is the last term obtained.
type NormalizeResult =
    | NormalForm of Expr
    | LimitReached of Expr

let isIdentStart (c: char) = 
    Char.IsLetter(c) || c = '_'

let isIdentChar (c: char) = 
    Char.IsLetterOrDigit(c) || c = '_' || c = '\''  

/// Parses a lambda term from a string.
///
/// Accepted concrete syntax:
/// - variable: x
/// - abstraction: \x.body or λx.body
/// - abstraction with several parameters: \x y z.body
///   which is treated as \x.\y.\z.body
/// - application: f x y
///   which is parsed left-associatively as ((f x) y)
/// - parentheses: ( ... )
///
/// Identifier rules:
/// - the first character of an identifier must be a letter or '_';
/// - subsequent characters may be letters, digits, '_', or '\''.
///
/// Separation rules:
/// - identifiers in an application are separated by whitespace or parentheses;
/// - parameters after '\' or 'λ' are separated by whitespace;
/// - the parameter list is separated from the body by '.';
/// - optional whitespace is allowed between tokens.
///
/// Returns:
/// - Some expr if parsing succeeded and the whole input was consumed;
/// - None if the input is empty, malformed, or contains an invalid suffix.
///
/// Examples of valid input:
/// - "x"
/// - "\x.x"
/// - "\x y.x"
/// - "f x y"
/// - "(\x.x) y"
///
/// Examples of invalid input:
/// - "\.x"
/// - "\x."
/// - "1x"
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
            if (text[i]) = '(' then
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

let rec freeVars expr = 
    match expr with
    | Var x -> 
        Set.singleton x 
    | App (l, r) -> 
        Set.union (freeVars l ) (freeVars r)
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
            Abs (x, substitute body varName replacement)

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

/// Reduces a term using normal-order beta-reduction, but no more than `limit` steps.
///
/// Strategy:
/// - the leftmost outermost redex is reduced first;
/// - alpha-conversion is used during substitution to avoid capture of free variables.
///
/// Parameters:
/// - limit: maximum number of beta-reduction steps to perform;
/// - expr: the term to normalize.
///
/// Returns:
/// - NormalForm expr if a normal form was reached within the limit;
/// - LimitReached expr if the limit was exhausted before normalization finished,
///   where expr is the last term produced.
///
/// Notes:
/// - if the term has no normal form, this function does not loop forever;
///   it stops after `limit` steps;
/// - if `limit <= 0`, the result is immediately LimitReached expr.
let rec normalizeWithLimit limit expr = 
    let rec loop steps current = 
        if steps <= 0 then 
            LimitReached current
        else
            match reduceOnceNormal current with 
            | Some(next) -> loop (steps - 1) next
            | None -> NormalForm current

    loop limit expr

/// Converts an expression to its textual representation.
///
/// Output format:
/// - variables are printed as their names;
/// - abstractions are printed as \x.body;
/// - applications are printed with left associativity;
/// - parentheses are added only when needed to preserve structure.
///
/// Examples:
/// - Var "x" -> "x"
/// - Abs("x", Var "x") -> "\x.x"
/// - App(Var "f", Var "x") -> "f x"
/// - App(Abs("x", Var "x"), Var "y") -> "(\x.x) y"
let rec toString expr = 
    let rec go prec e =
        match e with
        | Var x ->
            x
        | Abs(x, body) ->
            let s = "\\" + x + "." + go 0 body
            if prec > 0  then ("(" + s + ")") else s
        | App(l, r) -> 
            let s = go 1 l + " " + go 2 r
            if prec > 1 then "(" + s + ")" else s

    go 0 expr

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