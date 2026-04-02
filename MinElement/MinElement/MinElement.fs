// <copyright file="Program.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

/// Function for finding the minimum element in a list.
module MinElement

/// Returns the smallest element of the list; for an empty list, returns None.
let minInList lst =
    match lst with
    | [] -> None
    | h :: t -> Some(lst |> List.reduce (fun currentMin x -> if x < currentMin then x else currentMin))
