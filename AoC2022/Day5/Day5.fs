module Day5

open CommonFuncs
let FilterStackLine (stackLine: string) =
    stackLine.Trim('\r').Trim('\n')
    |> Seq.toList
    |> Seq.zip [-1 .. (stackLine.Length-2)]
    |> Seq.toList
    |> List.filter (fun (x: int,_) -> (x)%4 = 0 )
    |> List.map snd
    |> List.map string

let ConvertToTupleList (inp: string list list) =
    inp 
    |> List.mapi (fun (i: int) (x: string list) -> x |> List.mapi (fun (j: int) (y: string) -> (i,j+1,y)))
    |> List.reduce (@)
    |> List.filter (fun (_, _, x: string) -> x <> " " )

let FindIndexOfMaxInCol (inp: (int * int * string) list) (column: int) (topXed: int): int =
    inp
    |> List.mapi (fun (i: int) (x: int * int * string) -> i, x ) 
    |> List.filter (fun (_ , ( _, x: int ,_) ) -> x = column )
    |> List.sortByDescending (fun (_ , ( x: int, _ ,_) ) -> x )
    |> (fun (x: (int * (int * int * string)) list) -> x[topXed - 1])
    |> fst

let MoveBoxOffest (fromCol: int) (toCol: int) (toPos: int) (list: (int * int * string) list): (int * int * string) list =
    let indexFrom: int = (FindIndexOfMaxInCol list fromCol toPos)
    let indexTo: int = FindIndexOfMaxInCol list toCol 1
    let (indexToHeight: int),  _, _ = list[indexTo]
    let boxOnNewLocation: int * int * string = 
        list[indexFrom] 
        |> (fun ((x: int), (y: int), (z: string)) -> (indexToHeight+1,toCol,z))
    (List.removeAt indexFrom list) @ [boxOnNewLocation]

let rec MoveBoxXtimes (fromCol: int) (toCol: int) (times: int) (toPos: int) (list: (int * int * string) list): (int * int * string) list =
    if times = 1 then
        list
        |> MoveBoxOffest fromCol toCol toPos
    else
        list
        |> MoveBoxOffest fromCol toCol toPos
        |> MoveBoxXtimes fromCol toCol (times-1) (if toPos = 1 then 1 else toPos-1)

let DoMoves (useCrane: bool) (list: (int * int * string) list) (moveList: int list list): (int * int * string) list =
    (moveList)
    |> List.fold 
        (fun (accList: (int * int * string) list) (moves: int list) -> 
            accList
            |> MoveBoxXtimes (moves[1]) (moves[2]) moves[0] (if useCrane then moves[0] else 1)
        ) list

let runDay5: string =
    let input: string list = 
        (ReadDayText 5 "\r\n\r\n")

    let amountOfStacks: int = ((input[0])[((input[0]).Length - 2)]) |> string |> int

    let stacks: (int * int * string) list = 
        input[0].Split([|"\r\n"|], System.StringSplitOptions.None)
        |> Array.map FilterStackLine 
        |> Seq.toList
        |> List.rev
        |> ConvertToTupleList

    let moves: int list list = 
        input[1].Split([|"\r\n"; "move "; " from "; " to "|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> List.map int
        |> List.chunkBySize 3
    
    let a: (int * int * string) list = (DoMoves false stacks moves)[3..]
    let b: string = 
        [1..amountOfStacks]
        |> List.map (fun (x: int) -> List.filter (fun (_,c: int,_) -> c = x) a)
        |> List.map (fun (x: (int * int * string) list) -> 
                    x
                    |> List.maxBy (fun (h: int, _, _) -> h)
                    |> fun (_, _, v: string) -> v
                    ) 
        |> String.concat ""
    
    wl $"Part 1: {b}"

    let c: (int * int * string) list = (DoMoves true stacks moves)[3..]
    let d: string = 
        [1..amountOfStacks]
        |> List.map (fun (x: int) -> List.filter (fun (_,c: int,_) -> c = x) c)
        |> List.map (fun (x: (int * int * string) list) -> 
                    x
                    |> List.maxBy (fun (h: int, _, _) -> h)
                    |> fun (_, _, v: string) -> v
                    ) 
        |> String.concat ""
    
    wl $"Part 2: {d}"
    ""