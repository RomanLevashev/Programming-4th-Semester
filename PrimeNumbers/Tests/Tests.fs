// <copyright file="Tests.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module Tests

open FsUnit.Xunit
open PrimeNumbers
open Xunit

[<Fact>]
let ``isPrime detects prime and composite numbers`` () =
    [ 2; 3; 5; 97; 997 ] |> List.forall isPrime |> should equal true
    [ -1; 0; 1; 4; 9; 100; 1001 ] |> List.exists isPrime |> should equal false

[<Fact>]
let ``primes starts with expected first primes`` () =
    let expected =
        [ 2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71 ]

    primes |> Seq.take expected.Length |> Seq.toList |> should equal expected

[<Fact>]
let ``primes returns a large prime by index`` () =
    primes |> Seq.item 999 |> should equal 7919

[<Fact>]
let ``first generated numbers are prime and increasing`` () =
    let firstHundred =
        primes |> Seq.take 100 |> Seq.toList

    firstHundred |> List.forall isPrime |> should equal true
    firstHundred |> List.pairwise |> List.forall (fun (a, b) -> a < b) |> should equal true
