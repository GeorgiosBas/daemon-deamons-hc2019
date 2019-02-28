#r "../packages/FsRandom/lib/net45/FsRandom.dll"

open FsRandom
open System
open System.Diagnostics
open System.IO
open System.Security.Cryptography

let rand =
    let csprng = RandomNumberGenerator.Create()
    let fRNG () =
        let arr = Array.zeroCreate 8
        csprng.GetBytes arr
        let n = BitConverter.ToUInt64(arr, 0)
        n, ()
    createState fRNG ()

let shuffle arr =
    let fShuffle = Array.shuffleInPlace arr
    Random.get fShuffle rand

[<Struct>]
type Orientation = H | V

let (|O|) x = match x with | "H" -> H | _ -> V

type Image = {
    Id: uint32
    O: Orientation
    Tags: string Set
}

let getO x = x.O

let rec pairs (x: _ []) = seq {
    let mutable i = 0
    while i < x.Length - 1 do
        yield [x.[i]; x.[i + 1]]
        i <- i + 2
}

type SlideShow = Image list []

let solveIt (data: string []) () =
    let parseLine idx l =
        let O(o) :: _ :: tags = List.ofArray l
        {Id = uint32 idx; O = o; Tags = set tags}
    let data = data |> Seq.mapi (fun idx x -> x.Split(' ') |> parseLine idx) |> Array.ofSeq

    let slideShows: SlideShow =
        let h, v = Array.partition (getO >> ((=) H)) data
        let h = h |> Array.map List.singleton
        shuffle v
        let v = v |> pairs |> Array.ofSeq

        let theHolyResult = Array.append h v
        shuffle theHolyResult

        theHolyResult

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
    let data = File.ReadLines x |> Seq.tail |> Array.ofSeq
    let outputFileName = Path.ChangeExtension(x, ".out")
    let theHolySolution = solveIt data |> timeIt x
    File.WriteAllLines(outputFileName, theHolySolution))
