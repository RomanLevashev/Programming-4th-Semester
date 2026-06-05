// <copyright file="AdvancedLambdaInterpreter.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module AdvancedLambdaInterpreter

open System.IO
open FParsec

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
/// Named lambda expression.
/// </summary>
type Definition =
    {
        Name: string
        Expression: Expr
    }

/// <summary>
/// A program statement.
/// </summary>
type Statement =
    /// <summary>Named expression definition.</summary>
    | DefinitionStatement of Definition

    /// <summary>Expression to evaluate.</summary>
    | ExpressionStatement of Expr

/// <summary>
/// Parsed input program.
/// </summary>
type Program = { Statements: Statement list }

/// <summary>
/// Result of normalization with a step limit.
/// </summary>
type NormalizeResult =
    /// <summary>Normal form was reached.</summary>
    | NormalForm of Expr

    /// <summary>Step limit was reached before normalization finished.</summary>
    | LimitReached of Expr

let private ws = spaces
let private str s = pstring s .>> ws
let private ch c = pchar c .>> ws

let private isIdentifierStart c =
    isLetter c

let private isIdentifierPart c =
    isLetter c || isDigit c || c = '_' || c = '\''

let private reservedLet =
    attempt (pstring "let" .>> notFollowedBy (satisfy isIdentifierPart))

let private identifier: Parser<string, unit> =
    notFollowedBy reservedLet
    >>. many1Satisfy2L isIdentifierStart isIdentifierPart "identifier"
    .>> ws

let private makeAbstractions parameters body =
    parameters |> List.foldBack (fun parameter acc -> Abs(parameter, acc)) <| body

let private makeApplications expressions =
    match expressions with
    | [] ->
        failwith "Application chain must contain at least one expression."
    | head :: tail ->
        tail |> List.fold (fun acc expr -> App(acc, expr)) head

let private expressionParser, expressionParserRef =
    createParserForwardedToRef<Expr, unit>()

let private abstractionParser =
    ch '\\' >>. many1 identifier .>> ch '.' .>>. expressionParser
    |>> fun (parameters, body) -> makeAbstractions parameters body

let private atomParser =
    (identifier |>> Var)
    <|> between (ch '(') (ch ')') expressionParser

let private applicationParser =
    pipe2
        (many1 atomParser)
        (opt abstractionParser)
        (fun atoms trailingAbstraction ->
            match trailingAbstraction with
            | Some abstraction -> makeApplications (atoms @ [ abstraction ])
            | None -> makeApplications atoms)

do
    expressionParserRef.Value <-
        attempt abstractionParser <|> applicationParser

let private definitionParser =
    pipe2
        (reservedLet >>. ws >>. identifier .>> str "=")
        expressionParser
        (fun name expression ->
            {
                Name = name
                Expression = expression
            })

let private expressionOnlyParser =
    ws >>. expressionParser .>> eof

let private definitionLineParser =
    ws >>. definitionParser .>> eof

let private isBlankLine (line: string) =
    System.String.IsNullOrWhiteSpace line

let private isDefinitionLine (line: string) =
    let trimmed = line.TrimStart()

    trimmed = "let"
    || trimmed.StartsWith("let ")
    || trimmed.StartsWith("let\t")

let private parseWith parser input =
    match run parser input with
    | Success(value, _, _) ->
        Microsoft.FSharp.Core.Ok value
    | Failure(errorMessage, _, _) ->
        Microsoft.FSharp.Core.Error errorMessage

let private splitProgramLines (input: string) =
    input.Replace("\r\n", "\n").Replace("\r", "\n").Split('\n')
    |> Array.map _.Trim()
    |> Array.filter (isBlankLine >> not)
    |> Array.toList

let private parseDefinitionLine line =
    parseWith definitionLineParser line

let private parseExpressionText text =
    parseWith expressionOnlyParser text

let private parseStatementLine line =
    if isDefinitionLine line then
        parseDefinitionLine line |> Result.map DefinitionStatement
    else
        parseExpressionText line |> Result.map ExpressionStatement

/// <summary>
/// Parses a lambda interpreter program.
/// </summary>
let parseProgram input =
    let rec parseStatements statements remainingLines =
        match remainingLines with
        | [] ->
            Microsoft.FSharp.Core.Ok { Statements = List.rev statements }
        | line :: tail ->
            parseStatementLine line
            |> Result.bind (fun statement -> parseStatements (statement :: statements) tail)

    splitProgramLines input
    |> parseStatements []

let private expandDefinitions definitions expression =
    let rec expandDefinition seen name =
        match Map.tryFind name definitions with
        | None ->
            Microsoft.FSharp.Core.Ok(Var name)
        | Some expression ->
            if Set.contains name seen then
                Microsoft.FSharp.Core.Error(sprintf "Cyclic definition found for '%s'." name)
            else
                expand (Set.add name seen) Set.empty expression

    and expand seen bound expression =
        match expression with
        | Var name ->
            if Set.contains name bound then
                Microsoft.FSharp.Core.Ok expression
            else
                expandDefinition seen name
        | App(left, right) ->
            expand seen bound left
            |> Result.bind (fun newLeft ->
                expand seen bound right
                |> Result.map (fun newRight -> App(newLeft, newRight)))
        | Abs(parameter, body) ->
            expand seen (Set.add parameter bound) body
            |> Result.map (fun newBody -> Abs(parameter, newBody))

    expand Set.empty Set.empty expression

/// <summary>
/// Returns the set of free variables.
/// </summary>
let rec freeVars expression =
    match expression with
    | Var name ->
        Set.singleton name
    | App(left, right) ->
        Set.union (freeVars left) (freeVars right)
    | Abs(parameter, body) ->
        Set.remove parameter (freeVars body)

let private freshName baseName forbidden =
    let rec loop index =
        let candidate =
            if index = 0 then
                baseName + "'"
            else
                baseName + "'" + string index

        if Set.contains candidate forbidden then
            loop (index + 1)
        else
            candidate

    loop 0

/// <summary>
/// Performs capture-avoiding substitution.
/// </summary>
let rec substitute expression variable replacement =
    match expression with
    | Var name when name = variable ->
        replacement
    | Var _ ->
        expression
    | App(left, right) ->
        App(substitute left variable replacement, substitute right variable replacement)
    | Abs(parameter, _) when parameter = variable ->
        expression
    | Abs(parameter, body) ->
        if not (Set.contains variable (freeVars body)) then
            expression
        elif Set.contains parameter (freeVars replacement) then
            let forbidden =
                Set.union (freeVars body) (freeVars replacement)
                |> Set.add variable

            let newParameter = freshName parameter forbidden
            let renamedBody = substitute body parameter (Var newParameter)
            Abs(newParameter, substitute renamedBody variable replacement)
        else
            Abs(parameter, substitute body variable replacement)

/// <summary>
/// Performs one normal-order beta-reduction step.
/// </summary>
let rec reduceOnceNormal expression =
    match expression with
    | App(Abs(parameter, body), argument) ->
        Some(substitute body parameter argument)
    | App(left, right) ->
        match reduceOnceNormal left with
        | Some reducedLeft ->
            Some(App(reducedLeft, right))
        | None ->
            reduceOnceNormal right
            |> Option.map (fun reducedRight -> App(left, reducedRight))
    | Abs(parameter, body) ->
        reduceOnceNormal body
        |> Option.map (fun reducedBody -> Abs(parameter, reducedBody))
    | Var _ ->
        None

/// <summary>
/// Normalizes an expression using normal-order beta-reduction.
/// </summary>
let normalizeWithLimit limit expression =
    let rec loop remainingSteps current =
        if remainingSteps <= 0 then
            LimitReached current
        else
            match reduceOnceNormal current with
            | Some next ->
                loop (remainingSteps - 1) next
            | None ->
                NormalForm current

    loop limit expression

/// <summary>
/// Converts an expression to text.
/// </summary>
let toString expression =
    let rec loop precedence expression =
        match expression with
        | Var name ->
            name
        | Abs(parameter, body) ->
            let text = "\\" + parameter + "." + loop 0 body
            if precedence > 0 then "(" + text + ")" else text
        | App(left, right) ->
            let text = loop 1 left + " " + loop 2 right
            if precedence > 1 then "(" + text + ")" else text

    loop 0 expression

/// <summary>
/// Expands named definitions and normalizes every expression in a program.
/// </summary>
let interpretProgramWithLimit limit program =
    let rec interpret statements definitions output =
        match statements with
        | [] ->
            output
            |> List.rev
            |> String.concat System.Environment.NewLine
            |> Microsoft.FSharp.Core.Ok
        | DefinitionStatement definition :: tail ->
            if Map.containsKey definition.Name definitions then
                Microsoft.FSharp.Core.Error(sprintf "Duplicate definition: %s" definition.Name)
            else
                interpret tail (Map.add definition.Name definition.Expression definitions) output
        | ExpressionStatement expression :: tail ->
            expandDefinitions definitions expression
            |> Result.bind (fun expandedExpression ->
                match normalizeWithLimit limit expandedExpression with
                | NormalForm normalForm ->
                    interpret tail definitions (toString normalForm :: output)
                | LimitReached lastExpression ->
                    Microsoft.FSharp.Core.Error(
                        sprintf "Reduction limit reached at: %s" (toString lastExpression)
                    ))

    interpret program.Statements Map.empty []

/// <summary>
/// Parses and normalizes a program from a string.
/// </summary>
let interpretStringWithLimit limit input =
    parseProgram input
    |> Result.bind (interpretProgramWithLimit limit)

/// <summary>
/// Parses and normalizes a program from a string.
/// </summary>
let interpretString input =
    interpretStringWithLimit 10000 input

/// <summary>
/// Parses and normalizes a program from a file.
/// </summary>
let interpretFileWithLimit limit path =
    File.ReadAllText path |> interpretStringWithLimit limit

/// <summary>
/// Parses and normalizes a program from a file.
/// </summary>
let interpretFile path =
    interpretFileWithLimit 10000 path
