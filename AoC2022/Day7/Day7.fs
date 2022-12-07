module Day7

open CommonFuncs

type File (name: string, size: int) =
    member this.Name: string = name
    member this.Size: int = size

type Directory (name: string, parent: Directory option) =
    static let rec traverseUp (dir:Directory): Directory =
        if dir.Parent = None then
            dir
        else
            traverseUp dir.Parent.Value

    member this.Name: string = name
    member this.Parent: Directory option = parent 
    member val Directories: Directory list = [] with get, set
    member val Files: File list = [] with get, set

    member this.GetDirSize: int = 
        (List.sumBy (fun (x: File)-> x.Size) this.Files) + (List.sumBy (fun (x: Directory) -> x.GetDirSize ) this.Directories)
    
    static member GetParent (dir:Directory): Directory = if dir.Parent = None then dir else dir.Parent.Value
    static member GetRoot (dir:Directory): Directory = traverseUp dir
    static member GetDir (dirName: string) (dir: Directory): Directory = List.find (fun (x: Directory) -> x.Name = dirName) dir.Directories

    static member AddDir (dirName: string) (dir:Directory): Directory = 
        dir.Directories <- dir.Directories @ [Directory(dirName, Some(dir))]
        dir
    static member AddFile (fileName:string) (fileSize:int) (dir: Directory): Directory = 
        dir.Files <- dir.Files @ [File(fileName, fileSize)]
        dir
    
let ChangeDirCmd (cmd: string) (dir: Directory): Directory = 
    match cmd with
    | (cmd: string) when cmd.Contains("/") -> Directory.GetRoot dir
    | (cmd: string) when cmd.Contains("..") -> Directory.GetParent dir
    | _ -> Directory.GetDir (cmd.Split(" ")[2]) dir

let DoCommand (cmd: string) (dir:Directory): Directory =
    match cmd with
    | (cmd: string) when cmd.Contains("$ cd") -> ChangeDirCmd cmd dir
    | (cmd: string) when cmd.Contains("$ ls") -> dir
    | (cmd: string) when cmd.Contains("dir ") -> Directory.AddDir (cmd.Split(" ")[1]) dir
    | _ -> Directory.AddFile (cmd.Split(" ")[1]) (int (cmd.Split(" ")[0])) dir

let rec GetDirectorySizes (dir: Directory) (acc: (string*int) list ): (string*int) list =
    if dir.Directories.Length = 0 then
        acc @ [(dir.Name, dir.GetDirSize)]
    else
        List.fold (fun (acc: (string * int) list) (x: Directory) -> GetDirectorySizes x acc) acc dir.Directories
        |> (@) [(dir.Name, dir.GetDirSize)]

let runDay7: string =
    let input: string list = 
        (ReadDayText 7 "\r\n")
    
    let root:Directory = 
        List.fold (fun (acc:Directory) (x: string) -> DoCommand x acc) (Directory("/", None)) input
        |> Directory.GetRoot

    let directorySizeLimit = 100000
    let res1: int = 
        GetDirectorySizes root []
        |> List.sumBy (fun (_, (x:int)) ->  if x <= directorySizeLimit then x else 0)

    wl $"Part 1: sum of directories less than {directorySizeLimit} is {res1}"

    let unusedSpace: int = 70000000 - root.GetDirSize
    let neededSpace: int = 30000000 - unusedSpace
    let res2 = 
        GetDirectorySizes root []
        |> List.filter (fun (_, x: int) -> x >= neededSpace)
        |> List.sortBy (fun (_, x: int) -> x )
        |> fun (x: (string * int) list) -> snd x[0]
    wl $"Part 2: the smallest directory that will free up atleast {neededSpace} is {res2} big"
    ""