module Day1

open CommonFuncs

let sumStringOfCalories (input: string): int =
    List.sumBy (fun (x:string) -> (tryParseInt x)) (Array.toList (input.Split("\r\n")))

let collapseListOfStringOfCalories (listOfCals: string list): int list =
    List.map(fun (x: string) -> sumStringOfCalories x ) listOfCals

let getTopX (list: int list)(x:int): int list =
    (List.sortDescending list)[0..(x-1)]

let runDay1: string =
    let listOfElfsTotalCalories = collapseListOfStringOfCalories (ReadDayText 1 "\r\n\r\n")
    let maxVal: int = getTopX listOfElfsTotalCalories 1 |> List.sum
    let sumTop3: int = getTopX listOfElfsTotalCalories 3 |> List.sum
    wl $"Elf with most is with {maxVal} calories"
    wl $"Top 3 elfs has {sumTop3} calories"
    "" 