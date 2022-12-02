module Day2

open CommonFuncs

type Sign =
    | Rock = 0
    | Paper = 1
    | Scissor = 2

let ConvertToSign (typedSign: string): Sign =
    match typedSign with
    | "A" | "X" -> Sign.Rock
    | "B" | "Y" -> Sign.Paper
    | "C" | "Z" -> Sign.Scissor
    | _  -> failwith "Should not happen"

type Outcome =
    | Win = 6
    | Draw = 3
    | Loss = 0

let ConvertToOutcome (typedOutcome: string): Outcome =
    match typedOutcome with
    | "X" -> Outcome.Loss   
    | "Y" -> Outcome.Draw
    | "Z" -> Outcome.Win
    | _  -> failwith "Should not happen"

let GetOutcomeValue (hands: Sign*Sign): int =
    let Outcome = match fst hands, snd hands with
                    | (h0: Sign), (h1: Sign) when (int h0) = ((int h1) - 1) %% 3 -> Outcome.Win
                    | (h0: Sign), (h1: Sign) when h0 = h1 -> Outcome.Draw
                    | _ -> Outcome.Loss
    let FinaleValue = (int Outcome) + ((int (snd hands)) + 1)
    FinaleValue

let GetPlayHands (knowledge: Sign*Outcome): Sign*Sign =
    let opp: Sign = fst knowledge
    let know: Outcome = snd knowledge
    match know with
    | Outcome.Draw -> (opp, opp)
    | Outcome.Win ->  (opp, opp |> int |> (fun (x: int) -> x+1) |> (fun (x: int) -> x %% 3) |> string |> Sign.Parse)
    | Outcome.Loss -> (opp, opp |> int |> (fun (x: int) -> x-1) |> (fun (x: int) -> x %% 3) |> string |> Sign.Parse)
    | _ -> failwith "Should not happen"                        

let runDay2: string =
    let input: string list = (ReadDayText 2 "\r\n")
    let assumedSumOfPlay: int = 
        List.map (fun (x: string) -> x.Split(" ")) input |>
        List.map (fun (x: string[]) -> (ConvertToSign(x[0]), ConvertToSign(x[1]))) |> 
        List.map GetOutcomeValue |> 
        List.sum
    let sumOfPlay: int =
        List.map (fun (x: string) -> x.Split(" ")) input |>
        List.map (fun (x: string[]) -> (ConvertToSign(x[0]), ConvertToOutcome(x[1]))) |>
        List.map GetPlayHands |>
        List.map GetOutcomeValue |>
        List.sum  
    wl $"Following the assumed tactics gives the score sum {assumedSumOfPlay}"
    wl $"Following the given tactics gives the score sum {sumOfPlay}"
    ""