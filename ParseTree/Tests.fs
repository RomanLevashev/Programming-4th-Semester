// <copyright file="Tests.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module Tests

open FsCheck
open ParseTree

let prop_same (expr: Expr) =
    evalTail expr = eval expr

Check.Verbose prop_same