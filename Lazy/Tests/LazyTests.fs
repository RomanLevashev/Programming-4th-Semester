// <copyright file="LazyTests.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module LazyTests

open System.Threading
open FsUnit.Xunit
open Lazy
open Xunit

let private implementations =
    [
        "single-threaded", createSingleThreaded
        "multi-threaded", createMultiThreaded
        "lock-free", createLockFree
    ]

[<Fact>]
let ``first Get evaluates supplier and returns its result`` () =
    implementations
    |> List.iter (fun (name, createLazy) ->
        let mutable supplierCalls = 0
        let mutable suppliedValue = null

        let lazyValue =
            createLazy (fun () ->
                supplierCalls <- supplierCalls + 1
                let value = obj ()
                suppliedValue <- value
                value)

        let result = lazyValue.Get()

        Assert.Same(suppliedValue, result)
        supplierCalls |> should equal 1)

[<Fact>]
let ``sequential Get calls return the same object and do not recalculate`` () =
    implementations
    |> List.iter (fun (name, createLazy) ->
        let mutable supplierCalls = 0
        let lazyValue =
            createLazy (fun () ->
                supplierCalls <- supplierCalls + 1
                obj ())

        let first = lazyValue.Get()
        let second = lazyValue.Get()

        Assert.Same(first, second)
        supplierCalls |> should equal 1)

[<Fact>]
let ``lazy computation can store null result`` () =
    implementations
    |> List.iter (fun (name, createLazy) ->
        let lazyValue = createLazy (fun () -> null)

        lazyValue.Get() |> should equal null
        lazyValue.Get() |> should equal null)

[<Fact>]
let ``multi-threaded lazy calls supplier only once`` () =
    let mutable supplierCalls = 0

    let lazyValue =
        createMultiThreaded (fun () ->
            Interlocked.Increment(&supplierCalls) |> ignore
            Thread.Sleep 100
            obj ())

    let results =
        [ 1..32 ]
        |> List.map (fun _ -> async { return lazyValue.Get() })
        |> Async.Parallel
        |> Async.RunSynchronously

    results |> Array.distinct |> Array.length |> should equal 1
    supplierCalls |> should equal 1

[<Fact>]
let ``lock-free lazy returns one winning object to concurrent callers`` () =
    let mutable supplierCalls = 0

    let lazyValue =
        createLockFree (fun () ->
            Interlocked.Increment(&supplierCalls) |> ignore
            Thread.Sleep 100
            obj ())

    let results =
        [ 1..32 ]
        |> List.map (fun _ -> async { return lazyValue.Get() })
        |> Async.Parallel
        |> Async.RunSynchronously

    results |> Array.distinct |> Array.length |> should equal 1
    supplierCalls |> should greaterThanOrEqualTo 1
