module Day8

open CommonFuncs

let GetVerticalListWithIndexes (inp: int list list) (columnIndex: int) (rowFromIndex: int) (rowToIndex: int): (int*int) list =
    let rec GetRowVal (inp: int list list) (rowIndex: int) (acc: (int*int) list) =
        if rowIndex = rowToIndex then
            acc @ [(rowIndex, inp[rowIndex][columnIndex])]
        else
            GetRowVal inp (rowIndex+1) (acc @ [(rowIndex, inp[rowIndex][columnIndex])])
    GetRowVal inp rowFromIndex []

let GetHorizontalListWithIndexs (inp: int list list) (rowIndex: int) (colFromIndex: int) (colToIndex: int): (int*int) list =
    let rec GetColVal (inp: int list list) (colIndex: int) (acc: (int*int) list) =
        if colIndex = colToIndex then
            acc @ [(colIndex, inp[rowIndex][colIndex])]
        else
            GetColVal inp (colIndex+1) (acc @ [(colIndex, inp[rowIndex][colIndex])])
    GetColVal inp colFromIndex []

let IsVisible (inp: int list list) (index: (int*int)): bool = 
    let LeftMostVisible = 
        GetHorizontalListWithIndexs inp (fst index) (0) (snd index)
        |> List.sortByDescending (fun (i: int, x: int) -> x, -i)
        |> (fun (x: (int * int) list) -> (fst (x[0])) = (snd index))

    let RightMostVisible = 
        GetHorizontalListWithIndexs inp (fst index) (snd index) (inp.Length-1)
        |> List.sortByDescending (fun (i: int, x: int) -> x, i)
        |> (fun (x: (int * int) list) -> (fst (x[0])) = (snd index))

    let TopMostVisible = 
        GetVerticalListWithIndexes inp (snd index) (0) (fst index)
        |> List.sortByDescending (fun (i: int, x: int) -> x, -i)
        |> (fun (x: (int * int) list) -> (fst (x[0])) = (fst index))

    let BotMostVisible = 
        GetVerticalListWithIndexes inp (snd index) (fst index) (inp[0].Length-1)
        |> List.sortByDescending (fun (i: int, x: int) -> x, i)
        |> (fun (x: (int * int) list) -> (fst (x[0])) = (fst index))

    LeftMostVisible || RightMostVisible || TopMostVisible || BotMostVisible

let ScenicScore (inp: int list list) (index: (int*int)): int = 
    let rec getScore (acc: int) (inp: (int*int) list): int =
        if acc = (inp.Length - 1) then
            acc
        else
            if inp |> (fun (x: (int * int) list) -> (snd (x[0])) > (snd (x[acc]))) then
                getScore (acc+1) inp
            else
                acc

    let LeftScenicScore = 
        GetHorizontalListWithIndexs inp (fst index) (0) (snd index)
        |> List.rev
        |> getScore 1

    let RightScenicScore = 
        GetHorizontalListWithIndexs inp (fst index) (snd index) (inp.Length-1)
        |> getScore 1

    let TopScenicScore = 
        GetVerticalListWithIndexes inp (snd index) (0) (fst index)
        |> List.rev
        |> getScore 1

    let BotScenicScore = 
        GetVerticalListWithIndexes inp (snd index) (fst index) (inp[0].Length-1)
        |> getScore 1

    (LeftScenicScore * RightScenicScore * TopScenicScore * BotScenicScore)

let GetListOfIndexToCheck (inpLen: int): (int*int) list =
    let b: int list = [1..inpLen-2]
    List.fold (fun (acc: (int * int) list) (x: int) -> acc @ (b |> List.map (fun (y: int) -> (x,y)))) [] b

let runDay8: string =
    let input: int list list =
        (ReadDayText 8 "\r\n")
        |> List.map Seq.toList
        |> List.map (List.map (string >> int))

    let indexsToCheck: (int * int) list = GetListOfIndexToCheck input.Length

    let visibleTrees: int = 
        List.map (fun (x: int * int) -> (IsVisible input x), x ) indexsToCheck
        |> List.sumBy (fun (x: bool, _) -> if x then 1 else 0)
        |> (+) (((2*input.Length) - 2) + ((2*input[0].Length) - 2))

    wl $"Part 1: amount of visible trees {visibleTrees}"

    let scenicScores: int list = 
        List.map (fun (x: int * int) -> ScenicScore input x ) indexsToCheck
        |> List.sortDescending

    wl $"Part 2: highest scenic score is {scenicScores[0]}"
    ""