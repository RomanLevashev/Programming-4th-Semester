open System

let findElement (x:int)  (lst:int list) : int option = 
    let rec loop i lst =
        match lst with
        | [] -> None
        | h :: t -> 
            if h = x then Some i
            else loop (i + 1) t
    loop 0 lst