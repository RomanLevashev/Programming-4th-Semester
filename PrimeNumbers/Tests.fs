// <copyright file="Tests.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module Tests

open FsCheck
open PrimeNumbers


let firstN n = primes |> Seq.take n |> Seq.toList

let isStrictlyIncreasing xs =
    xs
    |> List.pairwise
    |> List.forall (fun (a, b) -> a < b)

let prop_allArePrime (NonNegativeInt n) =
    let k = n % 100
    firstN k |> List.forall isPrime

let prop_increasing (NonNegativeInt n) = 
    let k = n % 100
    firstN k |> isStrictlyIncreasing

let prop_startsWith2 = 
    primes |> Seq.head = 2

Check.Quick prop_allArePrime
Check.Quick prop_increasing
Check.Quick prop_startsWith2
