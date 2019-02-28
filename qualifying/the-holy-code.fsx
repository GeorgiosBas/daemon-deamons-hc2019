#r "../packages/FsRandom/lib/net45/FsRandom.dll"

open FsRandom
open System
open System.Diagnostics
open System.IO
open System.Security.Cryptography

let iterCount = 10

let min3 x1 x2 x3 =
    if x1 < x2 then
        if x1 < x3 then
            x1
        else
            x3
    else
        if x2 < x3 then
            x2
        else
            x3

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
    Random.get fShuffle <| rand

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

let scoreIt (s: SlideShow) =
    let fScore (t1, t2) =
        let s1 = Set.difference t1 t2 |> Set.count
        let s2 = Set.intersect t1 t2 |> Set.count
        let s3 = Set.difference t2 t1 |> Set.count
        min3 s1 s2 s3
    let score =
        s
        |> Seq.map (Seq.map (fun {Tags = t} -> t) >> Set.unionMany)
        |> Seq.pairwise
        |> Seq.sumBy fScore
    Console.WriteLine score
    score

let solveIt (data: string []) () =
    let parseLine idx l =
        let O(o) :: _ :: tags = List.ofArray l
        {Id = uint32 idx; O = o; Tags = set tags}
    let data = data |> Seq.mapi (fun idx x -> x.Split(' ') |> parseLine idx) |> Array.ofSeq

    let h, v = Array.partition (getO >> ((=) H)) data
    let h = h |> Array.map List.singleton

    let fSlideShows _: SlideShow =

        shuffle v
        let v = v |> pairs |> Array.ofSeq

        let theHolyResult = Array.append h v
        shuffle theHolyResult

        theHolyResult

    let theHolySlideShow =
        Seq.initInfinite fSlideShows
        |> Seq.take iterCount
        |> Seq.maxBy scoreIt

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
