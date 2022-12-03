module CommonFuncs

let InputOfDay (day: int): string = 
    let cwd: string = System.IO.Directory.GetCurrentDirectory()
    $"{cwd}\\Day{day}\\input.txt"

let TestInputOfDay (day:int): string =
    let cwd: string = System.IO.Directory.GetCurrentDirectory()
    $"{cwd}\\Day{day}\\testinput.txt"

let ReadDayText (day: int) (seperator: string): string list = Array.toList (System.IO.File.ReadAllText(InputOfDay(day)).Split(seperator))
let ReadDayTestText (day: int) (seperator: string): string list = Array.toList (System.IO.File.ReadAllText(TestInputOfDay(day)).Split(seperator))

let tryParseInt (s: string): int = 
    try 
        s |> int
    with :? System.FormatException -> 
        0

let wl (inp: string): unit =
    printf "%s\n" inp

let wlp (var: 'a) : (string -> 'a) =
    let SubFunc (inp: string): 'a =
        printf "%s\n" inp
        var
    SubFunc

let FindCharInString (str: string): (char->string) =
    let SubFnc (inp: char): string =
        String.filter (fun (x:char) -> x = inp) str |> 
        (fun (x: string) -> if x.Length = 0 then "" else x)
    SubFnc

let (%%) (inp: int) (modulus: int): int =
    let rem: int = inp % modulus
    match rem with
    | (r: int) when r < 0 -> r + modulus
    | _ -> rem  