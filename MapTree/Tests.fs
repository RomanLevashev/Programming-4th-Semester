// <copyright file="Tests.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module Tests

open MapTree
open FsCheck

let prop_same tree =
    mapTreeTail ((+) 1) tree = Some (mapTree ((+) 1) tree)

Check.Verbose prop_same