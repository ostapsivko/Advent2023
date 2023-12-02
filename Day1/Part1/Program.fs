open System
open System.IO

let toNum line = 
    let strings =
        line
        |> Seq.filter System.Char.IsDigit
        |> Seq.map (fun c -> c.ToString())

    match (strings |> List.ofSeq) with
        | [value] -> value + value |> int
        | [first; second] -> first + second |> int
        | head :: tail -> (head + (tail |> List.last)) |> int
        | _ -> 0

let total input = 
    input
    |> Array.map toNum
    |> Array.sum

let lines = File.ReadAllLines("input.txt")

lines |> total |> Console.WriteLine