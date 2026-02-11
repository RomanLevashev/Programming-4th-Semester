open System

let fibonacci (n:int) : bigint  = 
    if n < 0 then invalidArg "n" "n must be >= 0"
    
    let rec loop (i: int) (a: bigint) (b: bigint) : bigint = 
        if i = n then a
        else loop (i + 1) b (a + b)

    loop 0 0I 1I

let n = Console.ReadLine() |> int

printfn "%A" (fibonacci n)