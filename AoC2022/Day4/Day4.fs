module Day4

open CommonFuncs

let GetComp (inp: string[]): bool*bool*bool*bool =
    let inpI: int[] = Array.map int inp
    let inp1ContainsInp2: bool = inpI[0] <= inpI[2] && inpI[1] >= inpI[3]
    let inp1OverlapLowerInp2: bool = inpI[0] <= inpI[2] && inpI[1] >= inpI[2]
    let inp1OverlapUpperInp2: bool = inpI[0] <= inpI[3] && inpI[1] >= inpI[3]
    let inp1OverlapInp2: bool = inp1OverlapLowerInp2 || inp1OverlapUpperInp2

    let inp2ContainsInp1: bool = inpI[2] <= inpI[0] && inpI[3] >= inpI[1]
    let inp2OverlapLowerInp1: bool = inpI[2] <= inpI[0] && inpI[2] >= inpI[1]
    let inp2OverlapUpperInp1: bool = inpI[3] <= inpI[0] && inpI[3] >= inpI[1]
    let inp2OverlapInp1: bool = inp2OverlapLowerInp1 || inp2OverlapUpperInp1

    inp1ContainsInp2, inp2ContainsInp1, inp1OverlapInp2, inp2OverlapInp1

let CheckIfRangeInRange (inp: string[]): int =
    match GetComp inp with
    | true, _, _, _ -> 1
    | _, true, _, _ -> 1
    | _ -> 0

let CheckIfAnyOverlap (inp: string[]): int =
    match GetComp inp with
    | true, _, _, _ -> 1
    | _, true, _, _ -> 1
    | _, _, true, _ -> 1
    | _, _, _, true -> 1
    | _ -> 0

let runDay4: string =
    let input: string[] list = 
        (ReadDayText 4 "\r\n")
        |> List.map (fun (x: string) -> x.Split([|','; '-'|]))

    wl "Part 1"
    input
    //|> List.map (fun (x: string[]) -> wlp x $"{x[0]} {x[1]} {x[2]} {x[3]}")
    |> List.sumBy CheckIfRangeInRange
    |> string |> fun (x: string) -> wl $"There are {x} overlapping"

    wl "Part 2"
    input
    //|> List.map (fun (x: string[]) -> wlp x $"{x[0]} {x[1]} {x[2]} {x[3]}")
    |> List.sumBy CheckIfAnyOverlap
    |> string |> fun (x: string) -> wl $"There are {x} with any overlap"
    ""