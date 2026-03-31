module PhoneBookDomainTests

open Xunit
open FsUnit.Xunit
open PhoneBookDomain

[<Fact>]
let ``add stores phone by name`` () =
    let book =
        empty
        |> add "Alice" "111"

    tryFindPhone "Alice" book |> should equal (Some "111")

[<Fact>]
let ``tryFindPhone returns None for missing name`` () =
    tryFindPhone "Alice" empty |> should equal None

[<Fact>]
let ``tryFindName finds name by phone`` () =
    let book =
        empty
        |> add "Alice" "111"
        |> add "Bob" "222"

    tryFindName "222" book |> should equal (Some "Bob")

[<Fact>]
let ``tryFindName returns None for missing phone`` () =
    let book =
        empty
        |> add "Alice" "111"

    tryFindName "999" book |> should equal None

[<Fact>]
let ``add overwrites phone for existing name`` () =
    let book =
        empty
        |> add "Alice" "111"
        |> add "Alice" "999"

    tryFindPhone "Alice" book |> should equal (Some "999")

[<Fact>]
let ``getAll returns entries sorted by name`` () =
    let book =
        empty
        |> add "Bob" "222"
        |> add "Alice" "111"

    getAll book |> should equal [ ("Alice", "111"); ("Bob", "222") ]

[<Fact>]
let ``serialize converts phone book to text`` () =
    let book =
        empty
        |> add "Bob" "222"
        |> add "Alice" "111"

    serialize book |> should equal "Alice\t111\r\nBob\t222"

[<Fact>]
let ``deserialize restores phone book from valid text`` () =
    let text = "Alice\t111\r\nBob\t222"

    deserialize text
    |> should equal (Some (empty |> add "Alice" "111" |> add "Bob" "222"))

[<Fact>]
let ``deserialize ignores empty lines`` () =
    let text = "Alice\t111\r\n\r\nBob\t222\r\n"

    deserialize text
    |> should equal (Some (empty |> add "Alice" "111" |> add "Bob" "222"))

[<Fact>]
let ``deserialize returns None for line without tab`` () =
    deserialize "Alice 111"
    |> should equal None

[<Fact>]
let ``deserialize returns None for line with too many parts`` () =
    deserialize "Alice\t111\textra"
    |> should equal None