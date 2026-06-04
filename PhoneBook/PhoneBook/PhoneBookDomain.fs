module PhoneBookDomain

open System

/// <summary>
/// Represents a contact name.
/// </summary>
type Name = Name of string

/// <summary>
/// Represents a phone number.
/// </summary>
type PhoneNumber = PhoneNumber of string

/// <summary>
/// Represents a phone book that maps names to phone numbers.
/// </summary>
type PhoneBook = PhoneBook of Map<Name, PhoneNumber>

/// <summary>
/// Converts a name to a string.
/// </summary>
let nameToString (Name name) =
    name

/// <summary>
/// Converts a phone number to a string.
/// </summary>
let phoneNumberToString (PhoneNumber phone) =
    phone

/// <summary>
/// An empty phone book.
/// </summary>
let empty : PhoneBook =
    PhoneBook Map.empty

/// <summary>
/// Adds a new entry to the phone book or updates the phone number
/// if the name already exists.
/// </summary>
let add name phone (PhoneBook book) =
    PhoneBook(Map.add name phone book)

/// <summary>
/// Tries to find a phone number by name.
/// </summary>
let tryFindPhone name (PhoneBook book) =
    Map.tryFind name book

/// <summary>
/// Tries to find a name by phone number.
/// </summary>
let tryFindName phone (PhoneBook book) =
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
let getAll (PhoneBook book) =
    book
    |> Map.toList
    |> List.sortBy (fun (name, _) -> nameToString name)

/// <summary>
/// Converts the phone book to text lines.
/// </summary>
let serializeLines book =
    book
    |> getAll
    |> List.map (fun (name, phone) -> $"{nameToString name}\t{phoneNumberToString phone}")

/// <summary>
/// Converts the phone book to its text representation.
/// </summary>
let serialize book =
    book
    |> serializeLines
    |> String.concat Environment.NewLine

/// <summary>
/// Tries to build a phone book from text lines.
/// Returns None if the input format is invalid.
/// </summary>
let deserializeLines lines =
    let folder state (line: string) =
        match state with
        | None ->
            None
        | Some book ->
            if String.IsNullOrWhiteSpace line then
                Some book
            else
                match line.Split('\t') with
                | [| name; phone |] ->
                    Some(add (Name name) (PhoneNumber phone) book)
                | _ ->
                    None

    lines
    |> Seq.fold folder (Some empty)

/// <summary>
/// Tries to build a phone book from its text representation.
/// Returns None if the input format is invalid.
/// </summary>
let deserialize (text: string) =
    text.Replace("\r\n", "\n").Split('\n')
    |> deserializeLines
