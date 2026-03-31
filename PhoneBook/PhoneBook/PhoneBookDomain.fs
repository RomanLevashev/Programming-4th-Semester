module PhoneBookDomain

open System

/// <summary>
/// Represents a phone book that maps a name to a phone number.
/// </summary>
type PhoneBook = Map<string, string>

/// <summary>
/// An empty phone book.
/// </summary>
let empty : PhoneBook =
    Map.empty

/// <summary>
/// Adds a new entry to the phone book or updates the phone number
/// if the name already exists.
/// </summary>
let add name phone book =
    Map.add name phone book

/// <summary>
/// Tries to find a phone number by name.
/// </summary>
let tryFindPhone name book =
    Map.tryFind name book

/// <summary>
/// Tries to find a name by phone number.
/// </summary>
let tryFindName phone book =
    book
    |> Map.toSeq
    |> Seq.tryPick (fun (name, storedPhone) ->
        if storedPhone = phone then
            Some name
        else
            None)

/// <summary>
/// Returns all phone book entries sorted by name.
/// </summary>
let getAll book =
    book
    |> Map.toList
    |> List.sortBy fst

/// <summary>
/// Converts the phone book to its text representation.
/// </summary>
let serialize book =
    book
    |> getAll
    |> List.map (fun (name, phone) -> $"{name}\t{phone}")
    |> String.concat Environment.NewLine

/// <summary>
/// Tries to build a phone book from its text representation.
/// Returns None if the input format is invalid.
/// </summary>
let deserialize (text: string) =
    let lines =
        text.Replace("\r\n", "\n").Split('\n')

    let folder state line =
        match state with
        | None ->
            None
        | Some book ->
            if String.IsNullOrWhiteSpace line then
                Some book
            else
                let parts = line.Split('\t')

                if parts.Length <> 2 then
                    None
                else
                    Some (add parts[0] parts[1] book)

    lines
    |> Array.fold folder (Some empty)