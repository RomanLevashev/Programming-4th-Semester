module PhoneBook.Tests

open System
open FsUnit.Xunit
open PhoneBookDomain
open Xunit

let private name value =
    Name value

let private phone value =
    PhoneNumber value

[<Fact>]
let ``add stores phone by name`` () =
    let book =
        empty
        |> add (name "Alice") (phone "111")

    tryFindPhone (name "Alice") book |> should equal (Some(phone "111"))

[<Fact>]
let ``tryFindPhone returns None for missing name`` () =
    tryFindPhone (name "Alice") empty |> should equal None

[<Fact>]
let ``tryFindName finds name by phone`` () =
    let book =
        empty
        |> add (name "Alice") (phone "111")
        |> add (name "Bob") (phone "222")

    tryFindName (phone "222") book |> should equal (Some(name "Bob"))

[<Fact>]
let ``tryFindName returns None for missing phone`` () =
    let book =
        empty
        |> add (name "Alice") (phone "111")

    tryFindName (phone "999") book |> should equal None

[<Fact>]
let ``add overwrites phone for existing name`` () =
    let book =
        empty
        |> add (name "Alice") (phone "111")
        |> add (name "Alice") (phone "999")

    tryFindPhone (name "Alice") book |> should equal (Some(phone "999"))

[<Fact>]
let ``getAll returns entries sorted by name`` () =
    let book =
        empty
        |> add (name "Bob") (phone "222")
        |> add (name "Alice") (phone "111")

    getAll book |> should equal [ (name "Alice", phone "111"); (name "Bob", phone "222") ]

[<Fact>]
let ``serialize converts phone book to text`` () =
    let book =
        empty
        |> add (name "Bob") (phone "222")
        |> add (name "Alice") (phone "111")

    let expected =
        String.concat Environment.NewLine [ "Alice\t111"; "Bob\t222" ]

    serialize book |> should equal expected

[<Fact>]
let ``serializeLines converts phone book to lines`` () =
    let book =
        empty
        |> add (name "Bob") (phone "222")
        |> add (name "Alice") (phone "111")

    serializeLines book |> should equal [ "Alice\t111"; "Bob\t222" ]

[<Fact>]
let ``deserialize restores phone book from valid text`` () =
    let text =
        String.concat Environment.NewLine [ "Alice\t111"; "Bob\t222" ]

    deserialize text
    |> should equal (Some(empty |> add (name "Alice") (phone "111") |> add (name "Bob") (phone "222")))

[<Fact>]
let ``deserializeLines ignores empty lines`` () =
    deserializeLines [ "Alice\t111"; ""; "Bob\t222"; "" ]
    |> should equal (Some(empty |> add (name "Alice") (phone "111") |> add (name "Bob") (phone "222")))

[<Fact>]
let ``deserialize returns None for line without tab`` () =
    deserialize "Alice 111"
    |> should equal None

[<Fact>]
let ``deserialize returns None for line with too many parts`` () =
    deserialize "Alice\t111\textra"
    |> should equal None
