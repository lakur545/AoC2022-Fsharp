module Day11

open CommonFuncs

type Monkey (number: int, items: int64 list, operator: int64 -> int64, divisor: int, targets: int*int, relief: bool) =
    let monkeyNumber: int = number 
    let operator: int64 -> int64 = operator
    let divisor: int = divisor
    let mutable hand: int64 = 0
    let mutable items: int64 list = items
    let mutable targets: int*int = targets
    let mutable inspectionCount: int64 = 0

    let ThrowTo (friends: Monkey list) = 
        if (int hand) % divisor = 0 then
            friends[fst targets].Catch(hand)
        else
            friends[snd targets].Catch(hand)
        items <- List.removeAt 0 items

    let Inspect (moder: int64) =
        inspectionCount <- (inspectionCount + (int64 1))     
        hand <- (items[0] |> operator)
        hand <- hand |> (fun (x: int64) ->  if relief then int64((float x) / 3.0) else x )
        hand <- hand % moder
        if inspectionCount < 0 then
            wl (string inspectionCount)
        else
            "" |> ignore

    member this.Catch (item: int64) = items <- items @ [item]
    member this.ThrowAll (friends: Monkey list) (moder: int) = 
        [1..items.Length] 
        |> List.iter (fun _ -> Inspect moder; ThrowTo friends)

    member this.Inspections = inspectionCount
    member this.NumberOfItems = items.Length
    member this.Divisor = divisor

let CreateMonkeyFromString (relief: bool) (inp: string) =
    let GetFunctionFromString (inp:string): int64 -> int64 =
        let parts: string list = inp.Split(" ") |> Seq.toList
        let operator: string = parts[parts.Length-2]
        let operand: string = parts[parts.Length-1]
        if operand="old" then
            if operator="*" then
                (fun (x: int64) -> x * x)
            else
                (fun (x: int64) -> x + x)
        else
            if operator="*" then
                (fun (x: int64) -> x * (int64 operand))
            else
                (fun (x: int64) -> x + (int64 operand))
    
    let split: string list = inp.Split("\r\n") |> Seq.toList
    let monkeyNumber: int = 
        (split[0].Split(" ")[1])
        |> fun (x: string) -> x[0..(x.Length-2)]
        |> int
    let items: int64 list = 
        (split[1].Split(": ")[1]).Split(", ")
        |> Seq.toList
        |> List.map (fun (x:string) -> int64 x)
    let operator: int64 -> int64 = GetFunctionFromString split[2]
    let divisor: int = 
        split[3] 
        |> (fun (x: string) -> 
            let y: string[] = x.Split(" ")  
            int y[y.Length-1]
        )
    let targets: int*int = 
        split[4..5] 
        |> List.map (fun (x: string) -> 
            let y: string[] = x.Split(" ") 
            int y[y.Length-1]
        )
        |> fun (x: int list) -> x[0], x[1]
    Monkey(monkeyNumber, items, operator, divisor, targets, relief)

let runDay11: string =
    let input: string list =
        (ReadDayText 11 "\r\n\r\n")

    let rounds: int = 20
    let monkeyList: Monkey list = 
        input
        |> List.map (CreateMonkeyFromString true)

    let moder: int = List.fold (fun (acc: int) (x: Monkey) -> acc*x.Divisor) 1 monkeyList
    //Do rounds
    [1..rounds]
    |> List.iter (fun (x:int) ->
                    monkeyList 
                    |> List.iter (fun (x: Monkey) -> x.ThrowAll monkeyList moder)                        
                )

    let xActive: int = 2
    let xMostActiveMonkeys: Monkey list = 
        monkeyList
        |> List.sortByDescending (fun (x: Monkey) -> x.Inspections)
        |> fun (x: Monkey list) -> x[0..(xActive-1)]
    let multVal: int64 = 
        xMostActiveMonkeys
        |> List.fold (fun (acc: int64) (x: Monkey) -> acc*(x.Inspections)) (int64 1)

    wl $"Part 1: the multiplied value of the {xActive} most active monkeys is {multVal}"

    let roundsP2: int = 10000
    let monkeyListP2: Monkey list = 
        input
        |> List.map (CreateMonkeyFromString false)

    let moderP2: int = List.fold (fun (acc: int) (x: Monkey) -> acc*x.Divisor) 1 monkeyListP2
    //Do rounds
    [1..roundsP2]
    |> List.iter (fun (x:int) ->
                    monkeyListP2 
                    |> List.iter (fun (x: Monkey) -> x.ThrowAll monkeyListP2 moderP2)                        
                )

    let xActiveP2: int = 2
    let xMostActiveMonkeysP2: Monkey list = 
        monkeyListP2
        |> List.sortByDescending (fun (x: Monkey) -> x.Inspections)
        |> fun (x: Monkey list) -> x[0..(xActiveP2-1)]
    let multValP2: int64 = 
        xMostActiveMonkeysP2
        |> List.fold (fun (acc: int64) (x: Monkey) -> acc*(x.Inspections)) (int64 1)

    wl $"Part 2: the multiplied value of the {xActiveP2} most active monkeys is {multValP2}"
    ""