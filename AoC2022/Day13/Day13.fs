module Day13

open CommonFuncs

let runDay13: string =
    let input: string list =
        (ReadDayTestText 13 "\r\n\r\n")
    let a =
        input
        |> List.map (fun x -> 
                            x.Split("\r\n") 
                            |> Seq.toList
                            |> List.map (fun x -> 
                                                x.Split(',') 
                                                |> Seq.toList
                                                |> List.map (fun x -> x.Split([|'[';']'|]) |> Seq.toList)
                                        ) 
                    )
    ""