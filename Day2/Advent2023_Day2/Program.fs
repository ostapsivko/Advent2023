open System
open System.IO

let input = 
    File.ReadAllLines("input.txt")
    |> Array.map (fun l -> (l[5..l.IndexOf(':') - 1], l[l.IndexOf(':') + 1..]))
    |> List.ofArray 

let colors = 
    Map(seq {(12, "red"); (13, "green"); (14, "blue")}) 

type Round = { Marbles : (int * string) list }

type Game = { Id : int
              Rounds : Round array }

let toMarblePair (s : string) =
    let numAndName = s.Split(' ')

    (numAndName[0] |> int, numAndName[1])

let toRound (roundStr : string) =
    let cubes = roundStr.Split(',', StringSplitOptions.TrimEntries)

    let marbles =
        cubes
        |> Array.map toMarblePair
        |> List.ofArray 

    { Marbles = marbles }

let toGame (gameStr : (string * string)) =
    let gameId, game = gameStr 
    
    let rounds = game.Split(';', StringSplitOptions.TrimEntries)

    let id = gameId |> int

    { Id = id; 
      Rounds = rounds
      |> Array.map toRound }

let checkCount (count, name) =
    colors 
    |> Map.tryFindKey (fun key color -> 
        Console.WriteLine($"color {name} count {count} -> color {color} max {key}")
        color = name && key >= count)
    |> Option.isSome

let byGemsCount game =
    let init = game.Rounds.Length

    let res = 
        game.Rounds
        |> Array.filter (fun r -> 
            match r with 
            | { Round.Marbles = marbles } -> 
                let l = marbles.Length
                
                let inter = 
                    marbles
                    |> List.filter checkCount
                
                inter |> List.map string |> List.iter Console.WriteLine
                
                inter.Length = l 
            )
    
    res.Length = init

Console.WriteLine("===============================")
input
|> List.map toGame
|> List.filter byGemsCount
|> List.fold (fun n g -> n + g.Id) 0
|> Console.WriteLine
Console.WriteLine("===============================")