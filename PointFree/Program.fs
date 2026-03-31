// ﻿<copyright file="Program.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

open FsCheck

let funcOrig x l =
    List.map (fun y -> y * x) l

let funcStep1 x l =
    List.map (fun y -> x * y) l

let funcStep2 x l =
    List.map ((*) x) l

let funcStep3 x =
    List.map ((*) x)

let funcPointFree =
    List.map << (*)

let prop_sameResult (x: int) (l: int list) =
    funcOrig x l = funcPointFree x l
   
Check.Quick prop_sameResult