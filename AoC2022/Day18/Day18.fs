module Day18

open CommonFuncs

let TryGetKey (map: Map<(int * int * int),int>) (key: (int * int * int)): bool =
    if (map.TryFind key) = None then
        false
    else 
        true

let GetListOfPockets (map: Map<(int * int * int),int>) =
    let GetListOfPocketsP2 (pos: (int * int * int)): (int * int * int) list =
        let (posX: int), (posY: int), (posZ: int) = pos
        let mutationList: (int*int*int) list = [(1,0,0);(-1,0,0);(0,1,0);(0,-1,0);(0,0,1);(0,0,-1)]
        let exposedSidesOfCube: (int * int * int) list = 
            mutationList
            |> List.map (fun (x: int, y: int, z: int) -> 
                            if TryGetKey map (posX + x, posY + y, posZ + z) then
                                (-1000, -1000, -1000)
                            else
                                (posX + x, posY + y, posZ + z)
                        )
            |> List.filter (fun (x: int * int * int) -> x <> (-1000, -1000, -1000))
            |> List.filter (fun (x: int, y: int, z: int) -> x > -1 && y > -1 && z > -1)
        exposedSidesOfCube
    GetListOfPocketsP2 

let runDay18: string =
    let input: (int * int * int) list =
        (ReadDayText 18 "\r\n")
        |> List.map (fun (x: string) -> 
                                    x.Split(",") 
                                    |> (fun (y: string[]) -> (int y[0], int y[1], int y[2])) 
                    )

    let inputMapped: Map<(int * int * int),int> =
        input
        |> List.map (fun (x: int * int * int) -> (x, 0))
        |> Map


    let nonUniquePockets: (int * int * int) list =
        input
        |> List.map (GetListOfPockets inputMapped)
        |> List.fold (fun (acc: (int * int * int) list) (x: (int * int * int) list) -> acc@x) []

    wl $"Part 1: {nonUniquePockets.Length} are exposed"

    let capturedPockets: ((int * int * int) * (int * int * int) list) list =
        nonUniquePockets
        |> List.distinct
        |> List.mapi (fun (i: int) (x: int * int * int) -> (x, GetListOfPockets inputMapped x))
        |> List.filter (fun (_, x: (int * int * int) list) -> x.Length = 0)

    wl $"Part 2: {nonUniquePockets.Length - (capturedPockets.Length*6)} are exposed to water"
    ""