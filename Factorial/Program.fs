open System

let factorial (n:int) : bigint =
    let rec accFc (n:int) (acc:bigint) : bigint =
        if n < 0 then invalidArg "n" "n must be >= 0"
        elif n <= 1 then acc
        else accFc (n - 1) (acc * bigint n)
    accFc n 1I

let n = Console.ReadLine() |> int 

printfn "%A" (factorial n)