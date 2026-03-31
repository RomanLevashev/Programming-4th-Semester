module Program

open System
open System.IO
open PhoneBookDomain
open PhoneBookStorage

/// <summary>
/// Represents a user command in the console interface.
/// </summary>
type Command =
    | Exit
    | Add
    | FindPhone
    | FindName
    | ShowAll
    | Save
    | Load
    | Help

/// <summary>
/// Reads a line from the console.
/// Returns an empty string if the input is null.
/// </summary>
let readLineOrEmpty () =
    match Console.ReadLine() with
    | null -> ""
    | text -> text

/// <summary>
/// Parses a user input string into a command.
/// Returns Help for an unknown command.
/// </summary>
let parseCommand (input: string) =
    match input.Trim().ToLowerInvariant() with
    | "0" | "exit" -> Exit
    | "1" | "add" -> Add
    | "2" | "find-phone" -> FindPhone
    | "3" | "find-name" -> FindName
    | "4" | "show" -> ShowAll
    | "5" | "save" -> Save
    | "6" | "load" -> Load
    | _ -> Help

/// <summary>
/// Prints the list of available commands.
/// </summary>
let printMenu () =
    printfn ""
    printfn "0 - exit"
    printfn "1 - add entry"
    printfn "2 - find phone by name"
    printfn "3 - find name by phone"
    printfn "4 - show all entries"
    printfn "5 - save to file"
    printfn "6 - load from file"
    printf "Choose command: "

/// <summary>
/// Prints all phone book entries.
/// </summary>
let printAll book =
    match getAll book with
    | [] ->
        printfn "Phone book is empty."
    | entries ->
        entries
        |> List.iter (fun (name, phone) ->
            printfn "%s -> %s" name phone)

/// <summary>
/// Runs the main console loop.
/// </summary>
let rec loop book =
    printMenu ()

    match readLineOrEmpty () |> parseCommand with
    | Exit ->
        printfn "Goodbye."

    | Add ->
        printf "Enter name: "
        let name = readLineOrEmpty ()
        printf "Enter phone: "
        let phone = readLineOrEmpty ()

        let newBook = add name phone book
        printfn "Entry added."
        loop newBook

    | FindPhone ->
        printf "Enter name: "
        let name = readLineOrEmpty ()

        match tryFindPhone name book with
        | Some phone ->
            printfn "Phone: %s" phone
        | None ->
            printfn "Entry not found."

        loop book

    | FindName ->
        printf "Enter phone: "
        let phone = readLineOrEmpty ()

        match tryFindName phone book with
        | Some name ->
            printfn "Name: %s" name
        | None ->
            printfn "Entry not found."

        loop book

    | ShowAll ->
        printAll book
        loop book

    | Save ->
        printf "Enter file path: "
        let path = readLineOrEmpty ()

        try
            saveToFile path book
            printfn "Data saved."
        with
        | :? IOException as ex ->
            printfn "I/O error: %s" ex.Message

        loop book

    | Load ->
        printf "Enter file path: "
        let path = readLineOrEmpty ()

        try
            match loadFromFile path with
            | Some loadedBook ->
                printfn "Data loaded."
                loop loadedBook
            | None ->
                printfn "Invalid file format."
                loop book
        with
        | :? IOException as ex ->
            printfn "I/O error: %s" ex.Message
            loop book

    | Help ->
        printfn "Unknown command."
        loop book

/// <summary>
/// Program entry point.
/// </summary>
[<EntryPoint>]
let main _ =
    printfn "Phone book"
    loop empty
    0