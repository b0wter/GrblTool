module FileAccess 

open System.IO

let readTextFile filename =
    System.IO.File.ReadAllText(filename)

let readTextFileAsLines filename =
    System.IO.File.ReadAllLines(filename) 