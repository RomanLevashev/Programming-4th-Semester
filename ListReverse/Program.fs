let listReverse (lst: 'a list) : 'a list = 
    let rec loop (current: 'a list) (acc: 'a list) : 'a list = 
        match current with
        | [] -> acc
        | h :: t -> loop t (h :: acc)
    loop lst []