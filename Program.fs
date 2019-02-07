// Learn more about F# at http://fsharp.org

open System
open System.Text.RegularExpressions
open System

let footer = [| "G00 Z0.0000"; "G00 Z0.0000"; "G00 X0Y0"; "M05" |]

(*
    Matches the different combinations of coordinates. 
    G01 Z0.0000 =>
        Full match	0-11	G01 Z0.0000
        Group 1.	0-3     G01
        Group 2.	4-11	Z0.0000
        Group 3.	4-11	Z0.0000
        Group 4.	4-5	    Z           <== Important!
        Group 5.	5-11    0.0000      <== Important!

    G01 X5.5214Y3.6292 =>
        Full match	12-30	G01 X5.5214Y3.6292
        Group 1.	12-15	G01
        Group 2.	16-30	X5.5214Y3.6292
        Group 3.	16-23	X5.5214
        Group 4.	16-17	X           <== Important!
        Group 5.	17-23	5.5214      <== Important!
        Group 6.	23-30	Y3.6292
        Group 7.	23-24	Y           <== Important!
        Group 8.	24-30	3.6292      <== Important!
*)  
let moveCommandRegex = Regex("(G\d\d)\s(((X|Y|Z)(\d.\d*))?((\w)(\d.\d*))?)")

(*
    Matches all (x, y, z) coordinates but no commands.
*)
let coordinateRegex =Regex("([X|Y|Z])(\d?\.?\d*)")

type AbsolutePoint = {
    X: double
    Y: double
}

type RelativeXPoint = {
    X: double
}

type RelativeYPoint = {
    Y: double;
}

type HeightPoint = {
    Z: double;
}

type Point =
    | Absolute of AbsolutePoint
    | RelativeX of RelativeXPoint
    | RelativeY of RelativeYPoint
    | Height of HeightPoint

type Command =
    | Fast of Point
    | Precise of Point
    | Unknown of string

type Content = {
    Header: string []
    Body: Command []
}

let regexLine (line: string) : Command =
    let regexCoordinates (s: string) : Point =
        let parts = coordinateRegex.Matches(s)
        let groups = parts |> Seq.map (fun x -> (x.Groups.[1].Value, x.Groups.[2].Value)) |> List.ofSeq
        match groups with
        | [ ("X", (x: string)); ("Y", (y: string)) ] -> Absolute { X = Double.Parse(x); Y = Double.Parse(y) }
        | [ ("X", (d: string)) ] -> RelativeX { X = Double.Parse(d) }
        | [ ("Y", (d: string)) ] -> RelativeY { Y = Double.Parse(d) }
        | [ ("Z", (d: string)) ] -> Height { Z = Double.Parse(d) }
        | _ -> failwith "Number of match groups cannot be interpreted."

    if line.StartsWith("G0") then
        let parts = moveCommandRegex.Match(line).Groups
                    |> Seq.map (fun x -> x.Value)

        let goto = line.Substring(0, 3)
        let rest = line.Remove(0, 3);
        match(goto) with
        | "G00" -> Precise (rest |> regexCoordinates)
        | "G01" -> Fast (rest |> regexCoordinates)
        | _ -> failwith (sprintf "Unkown Goto command: %s" goto) 
        
    else
        Unknown line

let parseFileContent (content: string []) : Content =
    let notAMove (s: string) = (not (s.StartsWith("G00"))) && (s.StartsWith("G01"))
    let header = content |> Array.takeWhile notAMove
    let body = content 
               |> Array.except header 
               |> Array.map (regexLine)
    do body |> Array.Reverse
    let body = body
               |> Array.skipWhile (fun s -> match s with 
                                            | Precise _ -> false 
                                            | _ -> true)
    do body |> Array.Reverse

    { Header = header; Body = body }

let pointToString (p: Point) =
    match p with
    | Absolute { X = x; Y = y } -> sprintf "X%.4fY%.4f" x y
    | RelativeX { X = x } -> sprintf "X%.4f" x
    | RelativeY { Y = y } -> sprintf "Y%.4f" y
    | Height { Z = z } -> sprintf "Z%.4f" z

let commandToString (c: Command) =
   match c with
   | Fast x -> sprintf "G01 %s" (x |> pointToString)
   | Precise x -> sprintf "G00 %s" (x |> pointToString)
   | Unknown s -> s

let serialize (header: string[], content: Content[]) : string[] =
    let bodies = content 
                 |> Array.collect (fun x -> x.Body)
                 |> Array.map commandToString

    Array.concat [ header; bodies ]

[<EntryPoint>]
let main argv =
    match argv with
    | [| source ; target |] ->
        let content =  source |> (FileAccess.getFilesWithExtension "nc" ) |> Array.map (FileAccess.readTextFileAsLines >> parseFileContent)
        let grouped = content |> Array.groupBy (fun x -> x.Header)

        do printfn "Found %i header%s." grouped.Length (if grouped.Length = 1 then "" else "s")

        let addfooter x = footer |> Array.append x
        grouped |> Array.map (serialize >> addfooter)
                |> Array.iteri (fun i content ->
                    let file = sprintf "merged_%i.nc" i
                    let path = FileAccess.combinePaths target file
                    content |> FileAccess.writeTextToFile path)
        0
    | _ ->
        do printfn "Please provide a source and target path."
        128
