open System

let fileName = "input.txt"

let path = System.IO.Path.Combine(__SOURCE_DIRECTORY__, fileName)

let input = System.IO.File.ReadAllLines(path)

let processCard (card : string) =
    let parts = card[card.IndexOf(':') + 1..].Split('|', StringSplitOptions.TrimEntries)

    let winNumbers =
        parts[0].Split(' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int
        |> Set.ofArray

    let numbers = 
        parts[1].Split(' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int

    numbers
    |> Array.filter (fun value -> winNumbers.Contains value) 
    |> Array.length
    
input 
|> Array.map processCard
|> Array.filter (fun num -> num = 0 |> not)
|> Array.fold (fun acc value -> (acc + pown 2 (value - 1))) 0
|> Console.WriteLine