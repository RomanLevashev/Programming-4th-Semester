// <copyright file="Lazy.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module Lazy

open System.Threading

/// <summary>
/// Represents a lazy computation.
/// </summary>
type ILazy<'a> =
    abstract member Get: unit -> 'a

[<AllowNullLiteral>]
type private ValueHolder<'a>(value: 'a) =
    member _.Value = value

let private validateSupplier supplier =
    if isNull (box supplier) then
        nullArg (nameof supplier)

/// <summary>
/// Single-threaded lazy computation without synchronization.
/// </summary>
type SingleThreadedLazy<'a>(supplier: unit -> 'a) =
    do validateSupplier supplier

    let mutable valueHolder: ValueHolder<'a> = null

    interface ILazy<'a> with
        member _.Get() =
            if isNull valueHolder then
                valueHolder <- ValueHolder(supplier())

            valueHolder.Value

/// <summary>
/// Thread-safe lazy computation. The supplier is called no more than once.
/// </summary>
type MultiThreadedLazy<'a>(supplier: unit -> 'a) =
    do validateSupplier supplier

    let lockObject = obj ()
    let mutable valueHolder: ValueHolder<'a> = null

    interface ILazy<'a> with
        member _.Get() =
            let current = Volatile.Read(&valueHolder)

            if not (isNull current) then
                current.Value
            else
                lock lockObject (fun () ->
                    if isNull valueHolder then
                        Volatile.Write(&valueHolder, ValueHolder(supplier()))

                    valueHolder.Value)

/// <summary>
/// Lock-free lazy computation.
/// The supplier can be called more than once, but all callers receive the same value.
/// </summary>
type LockFreeLazy<'a>(supplier: unit -> 'a) =
    do validateSupplier supplier

    let mutable valueHolder: ValueHolder<'a> = null

    interface ILazy<'a> with
        member _.Get() =
            let current = Volatile.Read(&valueHolder)

            if not (isNull current) then
                current.Value
            else
                let computed = ValueHolder(supplier())
                let actual = Interlocked.CompareExchange(&valueHolder, computed, null)

                if isNull actual then
                    computed.Value
                else
                    actual.Value

/// <summary>
/// Creates a single-threaded lazy computation.
/// </summary>
let createSingleThreaded supplier =
    SingleThreadedLazy(supplier) :> ILazy<_>

/// <summary>
/// Creates a thread-safe lazy computation that calls the supplier no more than once.
/// </summary>
let createMultiThreaded supplier =
    MultiThreadedLazy(supplier) :> ILazy<_>

/// <summary>
/// Creates a lock-free lazy computation.
/// </summary>
let createLockFree supplier =
    LockFreeLazy(supplier) :> ILazy<_>
