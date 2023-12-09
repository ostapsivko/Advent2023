let debug x =
    printfn "%A" x
    x

let generate (input: int64 array) =
    Seq.init (input.Length - 1) (fun i -> input[i + 1] - input[i])

[<TailCall>]
let rec calculateLast input =
    let newSequence = (generate input) |> Array.ofSeq
    let last = Array.last input

    match newSequence |> Array.sum with
    | 0L -> input |> Array.last
    | _ -> (calculateLast newSequence) + last

let calculate input =
    let rec calculateFirstNumbers input =
        let newSequence = (generate input) |> Array.ofSeq
        let first = Array.head newSequence

        match newSequence |> Array.sum with
        | 0L -> [| 0L |]
        | _ ->
            let next = calculateFirstNumbers newSequence
            let newValue = (first - (Array.head next))
            Array.append [| newValue |] next

    let result = calculateFirstNumbers input
    Array.head input - (Array.head result)

let solvep1 =
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> System.IO.File.ReadAllLines
    |> Array.map (fun line ->
        line.Split(' ', System.StringSplitOptions.TrimEntries)
        |> Array.map int64)
    |> Array.map (fun values -> calculateLast values)
    |> Array.sum

let solvep2 =
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> System.IO.File.ReadAllLines
    |> Array.map (fun line ->
        line.Split(' ', System.StringSplitOptions.TrimEntries)
        |> Array.map int64)
    |> Array.map (fun values -> calculate values)
    |> Array.sum

solvep1
solvep2
