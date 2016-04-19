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

    let Row row (grid : Grid) = grid.[row, *]
    let Column column  (grid : Grid) = grid.[*, column]
    let Box x y (grid : Grid) = 
        let startX = x / 3 * 3
        let startY = y /3 * 3
        grid.[startY..(startY+2), startX..(startX+2)] |> Seq.cast |> Seq.toArray

    let PossibleValues x y (grid : Grid) = 
        let unusedValues list = (Set [1..9]) - (Set list)
        let unusedRow = Row y grid |> Array.choose id |> unusedValues
        let unusedColumn = Column x grid |> Array.choose id |> unusedValues
        let unusedBox = Box x y grid |> Array.choose id |> unusedValues
        unusedRow |> Set.intersect unusedColumn |> Set.intersect unusedBox
        
    let solve (grid : Grid) =
        let solution : Grid = Array2D.copy grid
        let isValid x y = (solution |> Row y |> Array.toList |> ValidList)
                            && (solution |> Column x |> Array.toList |> ValidList)
                            && (solution |> Box x y |> Array.toList |> ValidList)

        let rec findSolution x y = 
            match (x, y) with
                | (8, 8) -> true
                | (_, _) -> let nextX, nextY = match (x, y) with
                                | (8, y) -> 0, (y + 1)
                                | (_, _) -> (x + 1), y
                            let currentSolution = solution.[y, x]
                            match currentSolution with
                                | Some(value) -> findSolution nextX nextY
                                | None -> let possibleValues = PossibleValues x y solution
                                          let rec explorePossibilities = function 
                                                | [] -> false
                                                | head :: tail -> solution.[y, x] <- Some head
                                                                  let valid = isValid x y
                                                                  let continuation = match valid with
                                                                                        | true -> findSolution nextX nextY
                                                                                        | false -> false
                                                                  match continuation with
                                                                        | true -> true
                                                                        | false -> solution.[y, x] <- None
                                                                                   explorePossibilities tail
                                          explorePossibilities (possibleValues |> Set.toList)                                
                            
                                      
        findSolution 0 0 |> ignore
        solution
                                   
    [<EntryPoint>]
    let main argv = 
        let grid = ReadGrid "../../example.txt"
        System.Console.Write (GridToString grid)
        System.Console.WriteLine ""
        System.Console.WriteLine "Solved:"
        solve grid |> GridToString |> System.Console.Write
        System.Console.WriteLine ""
        System.Console.ReadKey() |> ignore
        0 // return an integer exit code
