open System.IO
open System
open System.Diagnostics

[<Struct>]
type Orientation = H | V

let (|O|) x = match x with | "H" -> H | _ -> V

type Image = {
    Id: uint32
    O: Orientation
    Tags: string Set
}

type SlideShow = Image list list

let solveIt (data: string list) () =
    let parseLine idx l =
        let O(o) :: _ :: tags = List.ofArray l
        {Id = uint32 idx; O = o; Tags = set tags}
    let data = data |> List.mapi (fun idx x -> x.Split(' ') |> parseLine idx)

    let slideShows: SlideShow = data |> List.map List.singleton

    slideShows
    |> Seq.map (Seq.map (fun {Id = idx} -> string idx) >> String.concat " ") |> Seq.append [string slideShows.Length]

// MAGIC ENDS HERE

let timeIt label f =
    let sw = Stopwatch()
    sw.Start()
    let x = f()
    sw.Stop()
    printfn "Time for %s: %O" label sw.Elapsed
    x

Environment.CurrentDirectory
|> Directory.EnumerateFiles
|> Seq.filter (Path.GetExtension >> ((=) ".txt"))
|> Seq.map (Path.GetFileName)
|> Seq.iter (fun x -> 
    let data = File.ReadAllLines x |> List.ofArray |> List.tail
    let outputFileName = Path.ChangeExtension(x, ".out")
    let theHolySolution = solveIt data |> timeIt x
    File.WriteAllLines(outputFileName, theHolySolution))
