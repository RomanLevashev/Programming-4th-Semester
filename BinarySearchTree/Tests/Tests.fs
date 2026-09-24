// <copyright file="Tests.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module Tests

open System
open System.Collections
open BinarySearchTree
open Xunit

let private tree values =
    List.fold (fun (acc: BinarySearchTree<_>) value -> acc.Add value) (BinarySearchTree<_>()) values

[<Fact>]
let ``enumeration sorts values and ignores duplicates`` () =
    let result = tree [ 5; 2; 8; 1; 3; 7; 9; 5 ]
    Assert.Equal<int list>([ 1; 2; 3; 5; 7; 8; 9 ], Seq.toList result)
    Assert.True(result.Contains 7)
    Assert.False(result.Contains 6)

[<Theory>]
[<InlineData(1)>]
[<InlineData(8)>]
[<InlineData(5)>]
[<InlineData(42)>]
let ``removal handles leaf single child root and missing value`` value =
    let values = [ 5; 2; 8; 1; 3; 7 ]
    let original = tree values
    let removed = original.Remove value
    Assert.Equal<int list>(values |> List.filter ((<>) value) |> List.sort, Seq.toList removed)
    Assert.Equal<int list>(List.sort values, Seq.toList original)

[<Fact>]
let ``empty enumerator remains exhausted and current is invalid`` () =
    use iterator = (tree ([]: int list)).GetEnumerator()
    Assert.Throws<InvalidOperationException>(fun () -> iterator.Current |> ignore) |> ignore
    Assert.False(iterator.MoveNext())
    Assert.False(iterator.MoveNext())
    Assert.Throws<InvalidOperationException>(fun () -> iterator.Current |> ignore) |> ignore

[<Fact>]
let ``current is stable and reset restarts traversal`` () =
    use iterator = (tree [ 2; 1 ]).GetEnumerator()
    Assert.Throws<InvalidOperationException>(fun () -> iterator.Current |> ignore) |> ignore
    Assert.True(iterator.MoveNext())
    Assert.Equal(1, iterator.Current)
    Assert.Equal(1, iterator.Current)
    Assert.True(iterator.MoveNext())
    Assert.Equal(2, iterator.Current)
    Assert.False(iterator.MoveNext())
    Assert.Throws<InvalidOperationException>(fun () -> iterator.Current |> ignore) |> ignore
    iterator.Reset()
    Assert.Throws<InvalidOperationException>(fun () -> iterator.Current |> ignore) |> ignore
    Assert.True(iterator.MoveNext())
    Assert.Equal(1, iterator.Current)

[<Fact>]
let ``enumerators have independent state`` () =
    let source = tree [ 3; 1; 2 ]
    use first = source.GetEnumerator()
    use second = source.GetEnumerator()
    first.MoveNext() |> ignore
    first.MoveNext() |> ignore
    second.MoveNext() |> ignore
    Assert.Equal(2, first.Current)
    Assert.Equal(1, second.Current)

[<Fact>]
let ``non generic enumeration supports strings`` () =
    let source = tree [ "c"; "a"; "b" ] :> IEnumerable
    let iterator = source.GetEnumerator()
    try
        Assert.Throws<InvalidOperationException>(fun () -> iterator.Current |> ignore) |> ignore
        let values = ResizeArray<string>()
        while iterator.MoveNext() do
            values.Add(unbox iterator.Current)
        Assert.Equal<string list>([ "a"; "b"; "c" ], List.ofSeq values)
    finally
        (iterator :?> IDisposable).Dispose()
