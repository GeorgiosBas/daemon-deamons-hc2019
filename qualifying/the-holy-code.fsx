open System.IO
open System

[<Struct>]
type Orientation = H | V

type Image = {
    O: Orientation
    Tags: string Set
}

type SlideShow = Image * Image option

let solveIt data = raise <| NotImplementedException()

// MAGIC ENDS HERE

Environment.CurrentDirectory
|> Directory.EnumerateFiles
|> Seq.filter (Path.GetExtension >> ((=) ".txt"))
|> Seq.map (Path.GetFileName)
|> Seq.iter (fun x -> 
    let data = File.ReadAllLines x |> List.ofArray |> List.tail
    let outputFileName = Path.ChangeExtension(x, ".out.txt")
    let theHolySolution = solveIt data
    File.WriteAllLines(outputFileName, theHolySolution))
