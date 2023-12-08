open System

let cards =
    seq {
        'A'
        'K'
        'Q'
        'J'
        'T'
        '9'
        '8'
        '7'
        '6'
        '5'
        '4'
        '3'
        '2'
    }

let (|Five|Four|Full|Three|TwoPair|OnePair|HighCard|) (hand: 'a array) =

    let toCheck = hand |> Array.groupBy (fun c -> c)

    match toCheck.Length with
    | 1 -> Five
    | 4 -> OnePair
    | 3 when
        toCheck
        |> Array.tryFind (fun (k, v) -> v.Length = 3)
        |> Option.isSome
        ->
        Three
    | 3 -> TwoPair
    | 2 when
        toCheck
        |> Array.tryFind (fun (k, v) -> v.Length = 4)
        |> Option.isSome
        ->
        Four
    | 2 -> Full
    | _ -> HighCard

type Bid = { Hand: string; Value: uint64 }

let debug v =
    printfn "%A" v
    v

let orderBids bids =
    let groups =
        bids
        |> Seq.groupBy (fun b ->
            match b.Hand.ToCharArray() with
            | Five -> 7
            | Four -> 6
            | Full -> 5
            | Three -> 4
            | TwoPair -> 3
            | OnePair -> 2
            | HighCard -> 1)
        |> Seq.sortBy (fun (k, _) -> k)

    groups
    |> Seq.map (fun (_, v) -> v)
    |> Seq.map (fun bids ->
        bids
        |> Seq.sortByDescending (fun b ->
            b.Hand
            |> Seq.map (fun c -> (Seq.findIndex (fun k -> c = k) cards))
            |> List.ofSeq))
    |> Seq.collect (fun b -> b)
    |> debug

let solve =
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> System.IO.File.ReadAllLines
    |> Array.map (fun l -> l.Split(' '))
    |> Array.map (fun [| cards; bid |] -> { Hand = cards; Value = bid |> uint64 })
    |> Seq.ofArray
    |> orderBids
    |> Seq.mapi (fun i bid -> ((i + 1) |> uint64, bid))
    |> Seq.fold (fun acc (i, bid) -> (acc + i * bid.Value) |> uint64) 0UL
    |> printfn "%A"

solve
