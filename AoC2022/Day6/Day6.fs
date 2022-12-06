module Day6

open CommonFuncs

let rec movingWindow (inp: string) (windowSize:int) (index: int) (filterF:string -> bool): int =
    if (index + windowSize > inp.Length) then 
        -1
    else if filterF inp[index..(index + windowSize - 1)] then
        index
    else
        movingWindow inp windowSize (index+1) filterF

let FindFirstUnique (inp: string) (windowSize: int): int =
    movingWindow inp windowSize 0 (fun (x: string) -> (List.distinct (Seq.toList x)).Length = x.Length)


let runDay6: string =
    let input: string list = 
        (ReadDayText 6 "")

    let windowSize: int = 4
    let firstMarker:int = (FindFirstUnique input[0] windowSize) + (windowSize)
    wl $"The first marker appears after character {firstMarker}"

    let windowSize2: int = 14
    let firstMarker2:int = (FindFirstUnique input[0] windowSize2) + (windowSize2)
    wl $"The first start of message marker appears after character {firstMarker2}"
    ""