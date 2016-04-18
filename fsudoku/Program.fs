module fsudoku.program

    // Types
    type Value = Option<int>

    let public ValidList (list : Value list) = 
        let rec withinRange = function
            | [] -> true
            | head :: tail -> let headWithinRange = match head with
                                                    | None -> true
                                                    | Some x ->  x > 0 && x < 10
                              headWithinRange && withinRange(tail)
        (list.Length = 9) && (withinRange list)
    
    [<EntryPoint>]
    let main argv = 
        printfn "%A" argv
        0 // return an integer exit code
