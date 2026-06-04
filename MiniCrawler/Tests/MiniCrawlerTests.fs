// <copyright file="MiniCrawlerTests.fs" company="Roman Levashev">
// Copyright (c) Roman Levashev. All rights reserved.
// Licensed under the MIT License.
// </copyright>

module MiniCrawlerTests

open FsUnit.Xunit
open MiniCrawler
open Xunit

let private rootAddress = "http://root.test"

let private createDownloader pages address =
    async {
        match Map.tryFind address pages with
        | Some page -> return page
        | None -> return (failwithf "Unexpected page address: %s" address)
    }

[<Fact>]
let ``extractLinks returns only http links in assignment form`` () =
    let html =
        """
        <a href="http://first.test">first</a>
        <a href="https://ignored.test">https</a>
        <a href='http://ignored.test'>single quotes</a>
        <a class="link" href="http://ignored.test">attributes</a>
        <a href="http://second.test/path">second</a>
        """

    extractLinks html
    |> should equal [ "http://first.test"; "http://second.test/path" ]

[<Fact>]
let ``getLinkedPagesSizes downloads linked pages`` () =
    let pages =
        Map.ofList
            [
                (rootAddress,
                 """
                <a href="http://first.test">first</a>
                <a href="http://second.test">second</a>
                """)
                ("http://first.test", "abcd")
                ("http://second.test", "123456")
            ]

    let result =
        getLinkedPagesSizesWith (createDownloader pages) rootAddress
        |> Async.RunSynchronously

    result
    |> should equal
        [
            {
                Address = "http://first.test"
                Size = 4
            }
            {
                Address = "http://second.test"
                Size = 6
            }
        ]

[<Fact>]
let ``printLinkedPagesSizes writes page address and size`` () =
    let pages =
        Map.ofList
            [
                (rootAddress, """<a href="http://first.test">first</a>""")
                ("http://first.test", "hello")
            ]

    let output = ResizeArray<string>()

    printLinkedPagesSizesWith (fun line -> output.Add line) (createDownloader pages) rootAddress
    |> Async.RunSynchronously

    output |> Seq.toList |> should equal [ "http://first.test — 5" ]

[<Fact>]
let ``getLinkedPagesSizes downloads linked pages in parallel`` () =
    let rootHtml =
        [ 1..3 ]
        |> List.map (fun index -> sprintf "<a href=\"http://site%d.test\">site</a>" index)
        |> String.concat ""

    let stateLock = obj ()
    let mutable activeDownloads = 0
    let mutable maxActiveDownloads = 0

    let download address =
        async {
            if address = rootAddress then
                return rootHtml
            else
                lock stateLock (fun () ->
                    activeDownloads <- activeDownloads + 1
                    maxActiveDownloads <- max maxActiveDownloads activeDownloads)

                do! Async.Sleep 100

                lock stateLock (fun () ->
                    activeDownloads <- activeDownloads - 1)

                return "content"
        }

    getLinkedPagesSizesWith download rootAddress
    |> Async.RunSynchronously
    |> ignore

    Assert.True(maxActiveDownloads > 1)
