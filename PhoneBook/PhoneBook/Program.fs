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
    | Add of Name * PhoneNumber
    | FindPhone of Name
    | FindName of PhoneNumber
    | ShowAll
    | Save of string
    | Load of string
    | Help
    | Unknown

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
/// </summary>
let parseCommand (input: string) =
    let parts =
        input.Trim().Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun part -> part.Trim())

    match parts with
    | [| "0" |]
    | [| "exit" |] ->
        Exit
    | [| "help" |] ->
        Help
    | [| "add"; name; phone |] ->
        Add(Name name, PhoneNumber phone)
    | [| "find"; "phone"; "by"; name |] ->
        FindPhone(Name name)
    | [| "find"; "name"; "by"; phone |] ->
        FindName(PhoneNumber phone)
    | [| "show" |] ->
        ShowAll
    | [| "save"; path |] ->
        Save path
    | [| "load"; path |] ->
        Load path
    | _ ->
        Unknown

/// <summary>
/// Prints the list of available commands.
/// </summary>
let printHelp () =
    printfn "Commands:"
    printfn "  add <name> <phone>"
    printfn "  find phone by <name>"
    printfn "  find name by <phone>"
    printfn "  show"
    printfn "  save <path>"
    printfn "  load <path>"
    printfn "  help"
    printfn "  exit"

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
            printfn "%s -> %s" (nameToString name) (phoneNumberToString phone))

/// <summary>
/// Runs the main console loop.
/// </summary>
let rec loop book =
    printf "> "

    match readLineOrEmpty () |> parseCommand with
    | Exit ->
        printfn "Goodbye."

    | Add(name, phone) ->
        let newBook = add name phone book
        printfn "Entry added."
        loop newBook

    | FindPhone name ->
        match tryFindPhone name book with
        | Some phone ->
            printfn "Phone: %s" (phoneNumberToString phone)
        | None ->
            printfn "Entry not found."

        loop book

    | FindName phone ->
        match tryFindName phone book with
        | Some name ->
            printfn "Name: %s" (nameToString name)
        | None ->
            printfn "Entry not found."

        loop book

    | ShowAll ->
        printAll book
        loop book

    | Save path ->
        try
            saveToFile path book
            printfn "Data saved."
        with
        | :? IOException as ex ->
            printfn "I/O error: %s" ex.Message

        loop book

    | Load path ->
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
        printHelp ()
        loop book

    | Unknown ->
        printfn "Unknown command. Type 'help' to see available commands."
        loop book

printfn "Phone book"
printHelp ()
loop empty
