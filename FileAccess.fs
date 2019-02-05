module FileAccess 

let readTextFile (filename: string) =
    System.IO.File.ReadAllText(filename)

let readTextFileAsLines (filename: string) =
    System.IO.File.ReadAllLines(filename) 
    
let writeTextToFile(filename: string) (content: string[]) =
    do printfn "Writing to file %s." filename
    if filename |> System.IO.Path.GetDirectoryName |> System.IO.Directory.Exists then
        do printfn "Target directory exists."
    else
        do printfn "Target directory does not exist, will be created."
        let dir = filename |> System.IO.Path.GetDirectoryName
        do dir |> System.IO.Directory.CreateDirectory |> ignore
    System.IO.File.WriteAllLines(filename, content)

let getFilesWithExtension (extension: string) (path: string) =
    let pattern = if extension.StartsWith("*") then extension else sprintf "*.%s" extension
    let dir = System.IO.DirectoryInfo(path)
    dir.GetFiles(pattern)
    |> Array.map (fun x -> x.FullName)
    
let combinePaths (a: string) (b: string) =
    System.IO.Path.Combine(a, b)
