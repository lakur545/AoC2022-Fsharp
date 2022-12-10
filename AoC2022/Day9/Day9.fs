module Day9

open CommonFuncs

type Coordinate(x: int, y: int) =
    let mutable x: int = x
    let mutable y: int = y

    member this.X with get() = x and set(value: int) = x <- value
    member this.Y with get() = y and set(value: int) = y <- value
    member this.Tuple = (this.X, this.Y)

    static member op_Addition(p1: Coordinate, p2: Coordinate) =
        new Coordinate(p1.X + p2.X, p1.Y + p2.Y)


let GetNewHead (head: Coordinate) (move: Coordinate): Coordinate =
    head + move

let tupleDif (a: int*int) (b: int*int): (int*int) =
    let fDif = (fst a) - (fst b)
    let sDif = (snd a) - (snd b)
    (fDif, sDif)

let tupleAbsDifSum (a: int*int): int =
    abs(abs(fst a) - abs(snd a))

let FollowUp (tail: Coordinate) (head: Coordinate): Coordinate =
    let Floor (inp: int): int =
        if inp < 0 then
            -1
        else
            1
    let (hx: int), (hy: int) = head.Tuple
    let (tx: int), (ty: int) = tail.Tuple
    let xDif: int = hx - tx
    let yDif: int = hy - ty
    let xDifSign: int = if xDif < 0 then -1 else 1
    let yDifSign: int = if yDif < 0 then -1 else 1

    if abs(tx - hx) > 1 || abs(ty - hy) > 1 then
        if xDif = 0 then
            Coordinate((tx), (ty + yDifSign))
        elif yDif = 0 then
            Coordinate((tx + xDifSign), (ty))
        else
            Coordinate((tx + xDifSign), (ty + yDifSign))
    else
        tail
        
let GetNewTail (tail: Coordinate) (head: Coordinate): Coordinate =
    FollowUp tail head

let GetMoveFromString (inp: string): Coordinate =
    match inp with
    | "R" -> Coordinate(1, 0)
    | "L" -> Coordinate(-1, 0)
    | "U" -> Coordinate(0, 1)
    | "D" -> Coordinate(0, -1)
    | _ -> failwith "Not valid move"

let MoveHead (dir: string) (headNTailPos: Coordinate*Coordinate): Coordinate*Coordinate= 
    let headPos: Coordinate = fst headNTailPos
    let tailPos: Coordinate = snd headNTailPos

    let move: Coordinate =  GetMoveFromString dir

    let newHead: Coordinate = (GetNewHead headPos move)
    let newTail: Coordinate = (GetNewTail tailPos newHead)
    newHead, newTail

let MoveHeadX (dir: string) (times:int) (currentList: (Coordinate*Coordinate) list): (Coordinate*Coordinate) list = 
    let rec Do (times: int) (acc: (Coordinate*Coordinate) list) =
        let a: Coordinate * Coordinate = MoveHead dir acc[acc.Length-1]
        if times = 1 then
            acc@[a]
        else
            Do (times-1) (acc@[a])

    Do times currentList

let MoveRope (dir:string) (latestPos: Coordinate list): Coordinate list =
    let knots: int = latestPos.Length   
    let rec Do (iter: int) (acc: Coordinate list): Coordinate list =
        let newPos: Coordinate list = 
            if iter = 0 then
                [GetNewHead latestPos[iter] (GetMoveFromString dir)]
            else
                [GetNewTail latestPos[iter] acc[iter-1]]
        if iter = knots-1 then
            acc@newPos
        else
            acc@newPos
            |> Do (iter+1) 
            
    Do 0 []

let MoveRopeX (dir:string) (times: int) (acc: Coordinate list list) =
    let rec Do (times: int) (acc: Coordinate list list): Coordinate list list =
        if times = 1 then
            acc@[(MoveRope dir (acc[acc.Length-1]))]
        else
            acc@[(MoveRope dir (acc[acc.Length-1]))]
            |> Do (times-1)
    Do times acc

let runDay9: string =
    let input: string list =
        (ReadDayText 9 "\r\n")

    let amountOfKnotsP1: int = 2
    let knotsP1: Coordinate list = List.fold (fun (acc: Coordinate list) (x: int) -> acc@[Coordinate(0,0)]) [] [1..amountOfKnotsP1]
    let res1: int =
        input
        |> List.fold (fun (acc: Coordinate list list) (x: string) ->
                (acc, x)
                |> (fun (x: Coordinate list list, y: string) -> (y.Split(" ")[0], int (y.Split(" ")[1]), x))
                |||> MoveRopeX
            ) [knotsP1]
        |> List.map (fun (x: Coordinate list) -> x[x.Length-1])
        |> List.map (fun (x: Coordinate) -> x.Tuple)
        |> List.distinct
        |> (fun (x: (int * int) list) -> x.Length)

    wl $"Part 1: tail hits {res1} distinct coordinates"

    let amountOfKnotsP2: int = 10
    let knotsP2: Coordinate list = List.fold (fun (acc: Coordinate list) (x: int) -> acc@[Coordinate(0,0)]) [] [1..amountOfKnotsP2]
    let res2 =
        input
        |> List.fold (fun (acc: Coordinate list list) (x: string) ->
                (acc, x)
                |> (fun (x: Coordinate list list, y: string) -> (y.Split(" ")[0], int (y.Split(" ")[1]), x))
                |||> MoveRopeX
            ) [knotsP2]
        |> List.map (fun (x: Coordinate list) -> x[x.Length-1])
        |> List.map (fun (x: Coordinate) -> x.Tuple)
        |> List.distinct
        |> (fun (x: (int * int) list) -> x.Length)

    wl $"Part 2: tail hits {res2} distinct coordinates"
    ""