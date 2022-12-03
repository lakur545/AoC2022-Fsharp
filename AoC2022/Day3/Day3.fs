module Day3

open CommonFuncs

let valueMap: Map<char,int> = Map (List.zip ['a'..'z'] [1..26] |> List.append (List.zip ['A'..'Z'] [27..52]))

let FindCharInString (str: string): (char->string) =
    let SubFnc (inp: char): string =
        String.filter (fun (x:char) -> x = inp) str |> 
        (fun (x: string) -> if x.Length = 0 then "" else x)
    SubFnc

let SplitStringHalf (inp: string): string list =
    [inp[0..((inp.Length/2)-1)]; inp[(inp.Length/2)..]]

let FindCommonBetweenStrings(inp1: string) (inp2: string): string =
    List.map (fun (x: char) -> FindCharInString inp1 x) (Seq.toList inp2) |>
    List.reduce (fun (a: string) (b: string) -> a+b)

let ConvertStringWithValueMap (inp: string): int =
    List.map (fun (x: char) -> valueMap[x] ) (Seq.toList inp) |>
    List.sum

let MakeStringUnique (inp: string): string =
    inp |>
    Seq.toList |>
    Seq.distinct |>
    List.ofSeq |>
    List.map string |>
    String.concat ""

let runDay3: string =
    let input: string list = (ReadDayText 3 "\r\n")

    wl "part 1"
    input |>
    List.map SplitStringHalf|> 
    List.map (fun (x:string list) -> (FindCommonBetweenStrings x[0] x[1])) |> 
    List.map MakeStringUnique |>
    String.concat "" |>
    (fun (x: string) -> wl $"Value of {x} is {ConvertStringWithValueMap x}")

    wl "part 2"
    input |>
    List.chunkBySize 3 |>
    List.map (fun (x: string list) -> [(FindCommonBetweenStrings x[0] x[1]); x[2]]) |>
    List.map (fun (x: string list) -> [MakeStringUnique x[0]; x[1]]) |>
    List.map (fun (x: string list) -> (FindCommonBetweenStrings x[1] x[0])) |>
    List.map MakeStringUnique |>
    String.concat "" |>
    (fun (x: string) -> wl $"Value of {x} is {ConvertStringWithValueMap x}")
    ""