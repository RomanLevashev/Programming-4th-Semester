// <copyright file="Tests.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>


module Tests

open FsCheck
open EvenNumbers

let prop_equivalent (xs: int list) = 
    mapCounter xs = filterCounter xs && filterCounter xs = foldCounter xs

Check.Quick prop_equivalent