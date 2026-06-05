// <copyright file="MiniCrawler.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module MiniCrawler

open System.Net.Http
open System.Text.RegularExpressions

/// <summary>
/// Contains a web page address and the downloaded page size in characters.
/// </summary>
type PageSize =
    {
        Address: string
        Size: int
    }

let private linkRegex =
    Regex("<a href=\"(?<address>http://[^\"]+)\">", RegexOptions.Compiled)

let private formatPageSize pageSize =
    sprintf "%s — %d" pageSize.Address pageSize.Size

/// <summary>
/// Extracts links written in the form &lt;a href="http://..."&gt;.
/// </summary>
let extractLinks (html: string) =
    linkRegex.Matches(html)
    |> Seq.cast<Match>
    |> Seq.map (fun matchResult -> matchResult.Groups["address"].Value)
    |> Seq.toList

let private downloadString (client: HttpClient) (address: string) =
    async {
        let! content = client.GetStringAsync(address) |> Async.AwaitTask
        return content
    }

/// <summary>
/// Downloads all pages linked from the given page and returns their sizes in characters.
/// </summary>
let getLinkedPagesSizesWith download address =
    async {
        let! html = download address
        let links = extractLinks html

        let! pageSizes =
            links
            |> List.map (fun link ->
                async {
                    let! pageContent = download link

                    return
                        {
                            Address = link
                            Size = pageContent.Length
                        }
                })
            |> Async.Parallel

        return pageSizes |> Array.toList
    }

/// <summary>
/// Downloads all pages linked from the given page and returns their sizes in characters.
/// </summary>
let getLinkedPagesSizes address =
    async {
        use client = new HttpClient()
        return! getLinkedPagesSizesWith (downloadString client) address
    }

/// <summary>
/// Downloads all pages linked from the given page and writes their sizes.
/// </summary>
let printLinkedPagesSizesWith writeLine download address =
    async {
        let! pageSizes = getLinkedPagesSizesWith download address

        pageSizes
        |> List.map formatPageSize
        |> List.iter writeLine
    }

/// <summary>
/// Downloads all pages linked from the given page and prints their sizes.
/// </summary>
let printLinkedPagesSizesAsync address =
    async {
        use client = new HttpClient()
        return! printLinkedPagesSizesWith (printfn "%s") (downloadString client) address
    }

/// <summary>
/// Downloads all pages linked from the given page and prints their sizes.
/// </summary>
let printLinkedPagesSizes address =
    printLinkedPagesSizesAsync address |> Async.RunSynchronously
