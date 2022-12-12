module Day12

open CommonFuncs

let StartvalCon = -1
let EndValCon = 123-97
let mutable _seen: (int*int) list = []

let convertToHeight (inp: char): int =
    let charVal: int = int inp
    if charVal = 83 then
        StartvalCon
    elif charVal = 69 then
        EndValCon
    else 
        charVal - 97

let convertToChar (inp:int): char =
    if inp = StartvalCon then
        'S'
    elif inp = EndValCon then
        'E'
    else 
        char (inp+97)

let FindStart (inp: Map<(int * int),int>): (int*int) =
    Map.findKey (fun _ (y: int) -> y = StartvalCon) inp

let AlreadyBeenThere (pos: int*int): bool =
    _seen
    |> List.filter (fun (x: int * int) -> pos = x)
    |> fun (x: ((int * int)) list) -> x.Length <> 0

let IsGoal (pos: int*int) (map: Map<(int * int),int>): bool =
    map[pos] = EndValCon

let TryGetFromMap (entry: int*int) (map: Map<(int * int),int>): int =
    if Map.exists (fun (x: int * int) _ -> x = entry) map then
        map[entry]
    else
        1000

let rec MapPath (pos: int*int) (map: Map<(int * int),int>) (path: ((int * int) * int) list) (paths: ((int * int) * int) list list): ((int * int) * int) list list =
    if AlreadyBeenThere pos then
        paths@[]
    elif IsGoal pos map then
        paths@[path]
    else
        _seen <- _seen@[pos]
        let curPosVal: int = map[pos]
        let neighboursPos: (int*int) list = List.map2 (fun (x: int) (y: int) -> (((fst pos)+x),((snd pos)+y))) [1;-1;0;0] [0;0;1;-1]
        let neighboursVal: int list = List.map (fun (x: int * int) -> (TryGetFromMap x map)) neighboursPos
        let mutable a:((int * int) * int) list list = []
        for i: int32 in 0..3 do
            let b: ((int * int) * int) list list =     
                if (curPosVal + 1) < neighboursVal[i] then
                    MapPath pos map (path@[(pos, curPosVal)]) paths
                else
                    MapPath neighboursPos[i] map (path@[(pos, curPosVal)]) paths
            a <- a@b
        paths@a

let MapPaths (startPos: int*int) (map: Map<(int * int),int>): ((int * int) * int) list list=
    MapPath startPos map [] []

let runDay12: string =
    let input: string list =
        (ReadDayTestText 12 "\r\n")

    // Coords is Row, Col, top left corner i 0,0
    let heightMap: Map<(int * int),int> = 
        input
        |> List.mapi (fun (i: int) (x: string) -> 
                        x
                        |> Seq.toList
                        |> List.mapi (fun (j: int) (y: char) -> (i,j),(convertToHeight y))   
                    )
        |> List.reduce (fun (x: ((int * int) * int) list) (y: ((int * int) * int) list) -> x@y)
        |> Map.ofList

    let start: int * int = FindStart heightMap
    let pathsToTop: ((int * int) * char) list list = 
        MapPaths start heightMap
        |> List.sortBy (fun (x: ((int * int) * int) list) -> x.Length)
        |> List.map (fun (x: ((int * int) * int) list) -> x |> List.map (fun (x: int * int, y: int) -> (x, (convertToChar y))))
    wl $"Part 1: shortest path is {pathsToTop[0].Length} steps"
    ""