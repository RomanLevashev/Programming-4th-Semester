// ﻿<copyright file="Tests.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>


module Tests

open Xunit
open FsUnit.Xunit
open BracketSequence

[<Theory>]
[<InlineData("", true)>]
[<InlineData("()", true)>]
[<InlineData("[]", true)>]
[<InlineData("{}", true)>]
[<InlineData("([]{})", true)>]
[<InlineData("{[()]}", true)>]
[<InlineData("{abc[()]}", true)>]
[<InlineData("a(b[c]{d}e)f", true)>]
[<InlineData("(", false)>]
[<InlineData(")", false)>]
[<InlineData("(]", false)>]
[<InlineData("([)]", false)>]
[<InlineData("(()", false)>]
[<InlineData("())", false)>]
[<InlineData("{[}]", false)>]
[<InlineData("][", false)>]
let ``isCorrectBracketSequence returns expected result`` input expected =
    isCorrectBracketSequence input |> should equal expected