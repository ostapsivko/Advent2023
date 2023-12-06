open System

let input =
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> System.IO.File.ReadAllLines

let exceedsDistance (s: uint64) (d: uint64) (t: uint64) = s * t > d

module Part1 =
    let times =
        input[0]
            .Split(
                ' ',
                StringSplitOptions.RemoveEmptyEntries
                ||| StringSplitOptions.TrimEntries
            )
        |> Array.skip 1
        |> Array.map uint64

    let distances =
        input[1]
            .Split(
                ' ',
                StringSplitOptions.RemoveEmptyEntries
                ||| StringSplitOptions.TrimEntries
            )
        |> Array.skip 1
        |> Array.map uint64

    let rec iterate (acc: bool list) (s: uint64) (t: uint64) (d: uint64) =
        match s with
        | 0UL -> iterate acc (s + 1UL) (t - 1UL) d
        | _ when t > 0UL ->
            let exceeds = exceedsDistance s d t
            // Console.WriteLine($"exceeds: {exceeds}")
            iterate (exceeds :: acc) (s + 1UL) (t - 1UL) d
        | _ -> acc

module Part2 =
    let time =
        input[0].[input[ 0 ].IndexOf(':') + 1 ..]
            .Replace(" ", "")
        |> uint64

    let record =
        input[1].[input[ 1 ].IndexOf(':') + 1 ..]
            .Replace(" ", "")
        |> uint64

    let distances t =
        [ 0UL .. t ]
        |> List.map (fun p ->
            let s = p
            let time = t - p
            s * time)

    let beats r d = d > r

Part1.times
|> Array.zip Part1.distances
|> Array.map (fun (d, t) -> Part1.iterate [] 0UL t d)
|> Array.map (fun v -> v |> List.filter (fun b -> b) |> List.length)
|> Array.reduce (*)

Part2.distances Part2.time
|> List.filter (fun d -> Part2.beats Part2.record d)
|> List.length
