open System.IO
open System

let solveIt data = raise <| NotImplementedException()

Environment.CurrentDirectory
|> Directory.EnumerateFiles
|> Seq.filter (Path.GetExtension >> ((=) ".in"))
|> Seq.map (Path.GetFileName)
|> Seq.iter (fun x -> 
    let data = File.ReadAllLines x
    let outputFileName = Path.ChangeExtension(x, ".out")
    let theHolySolution = solveIt data
    File.WriteAllLines(outputFileName, theHolySolution))
