module Day4

open CommonFuncs

let CheckIfRangeInRange (inp: int[]): int =
    ((inp[0] <= inp[2] && inp[1] >= inp[3]) || (inp[2] <= inp[0] && inp[3] >= inp[1])) 
    |> BtoI

let CheckIfAnyOverlap (inp: int[]): int =
    ((inp[0] <= inp[3] && inp[1] >= inp[3]) || (inp[2] <= inp[1] && inp[3] >= inp[1])) 
    |> BtoI

let runDay4: string =
    let input: int[] list = 
        (ReadDayText 4 "\r\n")
        |> List.map (fun (x: string) -> x.Split([|','; '-'|]))
        |> List.map (fun (x: string[]) -> Array.map int x)

    wl "Part 1"
    input
    |> List.sumBy CheckIfRangeInRange
    |> fun (x: int) -> wl $"There are {x} overlapping"

    wl "Part 2"
    input
    |> List.sumBy CheckIfAnyOverlap
    |> fun (x: int) -> wl $"There are {x} with any overlap"
    ""