module CommonFuncs

let InputOfDay (day: int): string = 
    let cwd: string = System.IO.Directory.GetCurrentDirectory()
    $"{cwd}\\Day{day}\\input.txt"

let ReadDayText (day: int) (seperator: string): string list = Array.toList (System.IO.File.ReadAllText(InputOfDay(day)).Split(seperator))

let tryParseInt (s: string): int = 
    try 
        s |> int
    with :? System.FormatException -> 
        0

let wl (inp: string): unit =
    System.Console.WriteLine(inp)