open System
open System.Diagnostics
open System.IO

[<Struct>]
type Orientation = H | V

let (|O|) x = match x with | "H" -> H | _ -> V

type Image = {
    Id: uint32
    O: Orientation
    Tags: string Set
}

let rec fixHV (x: _ []) = seq {
    let mutable v = None
    for i = 0 to x.Length - 1 do
        match x.[i].O with
        | H -> yield [x.[i]]
        | V when v.IsSome ->
            yield [v.Value; x.[i]]
            v <- None
        | V -> v <- Some x.[i]
}

let solveIt (data: string []) () =
    let parseLine idx l =
        let O(o) :: _ :: tags = List.ofArray l
        {Id = uint32 idx; O = o; Tags = set tags}
    let data = data |> Seq.mapi (fun idx x -> x.Split(' ') |> parseLine idx) |> Array.ofSeq

    data |> Array.sortInPlaceBy (fun {Tags = x} -> x)

    let theHolySlideShow = data |> fixHV |> Array.ofSeq

    theHolySlideShow
    |> Seq.map (Seq.map (fun {Id = idx} -> string idx) >> String.concat " ") |> Seq.append [string theHolySlideShow.Length]

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
    let data = File.ReadLines x |> Seq.tail |> Array.ofSeq
    let outputFileName = Path.ChangeExtension(x, ".out")
    let theHolySolution = solveIt data |> timeIt x
    File.WriteAllLines(outputFileName, theHolySolution))
