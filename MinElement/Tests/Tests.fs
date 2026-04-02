// <copyright file="Tests.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module Tests

open FsCheck
open FsCheck.Xunit
open MinElement

[<Property>]
let ``minInList matches List.min on non-empty lists`` ((NonEmptyArray xs) : NonEmptyArray<int>) =
    let lst = Array.toList xs
    minInList lst = Some (List.min lst)

[<Property>]
let ``empty list returns None`` () =
    minInList [] = None