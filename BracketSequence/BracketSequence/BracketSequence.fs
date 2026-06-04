// <copyright file="BracketSequence.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module BracketSequence

/// Checks whether a string contains a correct bracket sequence.
///
/// The function ignores all non-bracket characters.
/// Supported bracket types are (), [], and {}.
///
/// A sequence is considered correct if:
/// - every opening bracket has a matching closing bracket of the same type;
/// - brackets are properly nested;
/// - no closing bracket appears before its matching opening bracket.
///
/// <param name="s">The input string to validate.</param>
/// <returns>
/// <c>true</c> if the bracket sequence in the input string is correct;
/// otherwise, <c>false</c>.
/// </returns>
let isCorrectBracketSequence (s: string) =
    let pairs =
        dict [
            ')', '('
            ']', '['
            '}', '{'
        ]

    let opening = set [ '('; '['; '{' ]

    let rec loop stack chars =
        match stack, chars with
        | [], [] ->
            true

        | _, [] ->
            false

        | stack, ch :: rest when opening.Contains ch ->
            loop (ch :: stack) rest

        | top :: stackTail, ch :: rest when pairs.ContainsKey ch && top = pairs[ch] ->
            loop stackTail rest

        | _, ch :: _ when pairs.ContainsKey ch ->
            false

        | stack, _ :: rest ->
            loop stack rest

    s |> Seq.toList |> loop []
