module Day10

open CommonFuncs

let DoCommand (cmd: string) (acc: (int * int) list): (int * int) list =
    let DoAddX (cmd: string) (curVal: int*int): (int * int) list =
        let addVal: int = (int<<string)(cmd.Split(" ")[1])
        [(((fst curVal) + 1), (snd curVal)); (((fst curVal) + 2), ((snd curVal) + addVal))]
        
    let currIter: int * int = acc[acc.Length-1]
    match cmd with
    | (cmd: string) when cmd.Contains("noop") -> (acc @ [(((fst currIter) + 1), (snd currIter))])
    | (cmd: string) when cmd.Contains("addx") -> (acc @ (DoAddX cmd currIter))
    | _ -> failwith "Unkown command"

let GetSpriteRangeAtCycle (registerCycles: (int*int) list) =
    let GetSpriteRangeAtCycleP2 (iter: int) =
        let valAtIter: int = (snd registerCycles[iter])
        [valAtIter - 1 ; valAtIter ; valAtIter + 1]
    GetSpriteRangeAtCycleP2

let ShouldPrint (printPos: int) (cycles: (int * int) list): bool =
    let curSpritePositions: int list = GetSpriteRangeAtCycle cycles printPos
    List.contains (printPos % 40) curSpritePositions

let runDay10: string =
    let input: string list =
        (ReadDayText 10 "\r\n")

    let cycles: (int * int) list =
        input|>
        List.fold (fun (acc: (int * int) list) (cmd: string) -> DoCommand cmd acc) [(0, 1)]
    
    let interestingDuringCycles: int list = [(20-1)..40..(220-1)]

    let res1: int =
        cycles
        |> List.filter (fun (x: int * int) -> List.contains (fst x) interestingDuringCycles )
        |> List.fold (fun (acc: int) (cycle: int, sigStr: int) -> acc + ((cycle+1) * sigStr) ) 0

    wl $"Part 1: sum of signals is {res1}"

    let printCycles: int list = [(1-1)..(240-1)]

    let res2: string list list =
        printCycles
        |> List.map (fun (x: int) -> if (ShouldPrint x cycles) then "#" else ".")
        |> List.chunkBySize 40
    
    wl "Part 2:"
    List.iter (fun (x: string list) -> x |> List.reduce (fun (x: string) (y: string) -> $"{x}{y}") |> wl) res2
    ""