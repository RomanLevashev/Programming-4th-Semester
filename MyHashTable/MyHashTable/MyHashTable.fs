// <copyright file="MyHashTable.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>


/// A simple hash table with external hash function.
module MyHashTable

/// A hash table that supports add, contains, and remove.
type MyHashTable<'T when 'T : equality>(size : int, hashFunc : 'T -> int) =

    let buckets : 'T list array = Array.create size []

    let index (value : 'T) =
        let h = hashFunc value
        ((h % size) + size) % size

    /// Adds a value to the table if it is not already present.
    member _.Add(value : 'T) =
        let i = index value
        if not (List.contains value buckets[i]) then
            buckets[i] <- value :: buckets[i]

    /// Returns true if the value is in the table.
    member _.Contains(value : 'T) =
        let i = index value
        List.contains value buckets[i]

    /// Removes a value from the table.
    member _.Remove(value : 'T) =
        let i = index value
        buckets[i] <- List.filter (fun x -> x <> value) buckets[i]