module fsudoku.program

    // Types
    type Value = Option<int>
    type Grid = Value[,]

    let public ValidList (list : Value list) = 
        let rec withinRange = function
            | [] -> true
            | head :: tail -> let headWithinRange = match head with
                                                    | None -> true
                                                    | Some x ->  x > 0 && x < 10
                              headWithinRange && withinRange(tail)
        let someList = list |> List.choose id
        (list.Length = 9) && (withinRange list) && (someList.Length = (List.distinct someList |> List.length))
    
    let public ReadGrid filename : Grid = 
       let intToOpt x = match x with
                            | 0 -> None
                            | x -> Some x
       // Is there some better way to do this?
       let irregular = System.IO.File.ReadLines(filename) 
                           |> Seq.map (fun line -> 
                                        line.Split ' '
                                        |> Seq.map System.Int32.Parse 
                                        |> Seq.map intToOpt
                                        |> Seq.toArray)
                           |> Seq.toArray
       Array2D.init 9 9 (fun x y -> irregular.[x].[y])

    let public GridToString (grid : Grid) = 
        let intOptionToString = function
                                | None -> "_"
                                | Some x -> string x
        let printRow row = 
            Array.map intOptionToString row 
                |> String.concat " " 
            
        [for row in 0..8 do yield grid.[row, *]]
            |> List.map printRow
            |> String.concat "\n"
                                                              
    [<EntryPoint>]
    let main argv = 
        let grid = ReadGrid "../../example.txt"
        System.Console.Write (GridToString grid)
        0 // return an integer exit code
