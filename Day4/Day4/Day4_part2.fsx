open System

let fileName = "input.txt"

let path = System.IO.Path.Combine(__SOURCE_DIRECTORY__, fileName)

let input = System.IO.File.ReadAllLines(path)

let cards = 
    seq { 1 .. input.Length - 1 }
    |> Seq.map (fun s -> (s, 1))
    |> Map.ofSeq

let mapUpdater = function 
    | Some v -> Some (v + 1)
    | None -> Some (1)

let mapUpdaterX x =  function 
    | Some v -> Some (v + 1 * x)
    | None -> Some (1)

let processCard(card : string)  (state : Map<int, int>) =
    let parts = card[card.IndexOf(':') + 1..].Split('|', StringSplitOptions.TrimEntries)

    let cardNumber = card[5 .. card.IndexOf(':') - 1] |> int

    let winNumbers =
        parts[0].Split(' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int
        |> Set.ofArray

    let numbers = 
        parts[1].Split(' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int

    let matches = 
        numbers
        |> Array.filter (fun value -> winNumbers.Contains value)

    // matches |> Array.iter (fun m -> Console.Write($"{m} "))
    // Console.WriteLine("=========")

    let count = 
        matches
        |> Array.length

    //increment current card count, because we have it
    let mutable newState = state.Change (cardNumber, mapUpdater)

    if count = 0 |> not then
    //update rest of the numbers
        //newState |> Map.iter (fun k v -> Console.WriteLine($"K:{k} V:{v}"))
        //Console.WriteLine(lineNumber)
        let multiplier = newState.Item cardNumber
        let idxStart = (cardNumber + 1)
        let idxEnd = idxStart + count - 1
    
        //Console.WriteLine($"count:{count} line number:{lineNumber}")
        Console.WriteLine($"start:{idxStart} end:{idxEnd}")
    
        for n in {idxStart .. idxEnd} do
            //Console.WriteLine($"n:{n} multiplier:{multiplier}")
            newState <- newState.Change (n, mapUpdaterX multiplier)

    newState

let mutable state = Map.empty

for line in input do    
    state <- processCard line state

// state
// |> Map.iter (fun k v -> Console.WriteLine($"K:{k} V:{v}"))

state
|> Map.fold (fun acc key value -> (acc + value)) 0
|> Console.WriteLine