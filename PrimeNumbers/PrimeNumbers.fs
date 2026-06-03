// <copyright file="PrimeNumbers.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module PrimeNumbers

/// <summary>
/// Checks whether a number is prime.
/// Returns true if the number is greater than 1 and has no divisors
/// other than 1 and itself.
/// </summary>
let isPrime n =
    if n < 2 then false
    else
        let limit = int (sqrt (float n))
        seq { 2 .. limit } |> Seq.forall (fun d -> n % d <> 0)

/// <summary>
/// An infinite sequence of prime numbers starting from 2.
/// </summary>
let primes =
    Seq.initInfinite ((+) 2) |> Seq.filter isPrime
