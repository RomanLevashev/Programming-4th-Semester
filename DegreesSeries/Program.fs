open System

let degreesSeries (n: int) (m: int) : bigint list = 
    if n < 0 then invalidArg "n" "n must be >= 0"
    if m < 0 then invalidArg "m" "m must be >= 0"

    let first = 1I <<< n 

    let rec loop i current lst = 
        if i > m then
            List.rev lst
        else
            loop (i + 1) (current * 2I) (current :: lst)

    loop 0 first []


let n = Console.ReadLine() |> int 
let m = Console.ReadLine() |> int

degreesSeries n m |> printfn "%A"
