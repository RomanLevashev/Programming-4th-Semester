module PhoneBookStorage

open System.IO
open PhoneBookDomain

/// <summary>
/// Saves the phone book to a file.
/// </summary>
let saveToFile path book =
    File.WriteAllLines(path, serializeLines book)

/// <summary>
/// Loads the phone book from a file.
/// Returns None if the file content has an invalid format.
/// </summary>
let loadFromFile path =
    File.ReadAllLines path
    |> deserializeLines
