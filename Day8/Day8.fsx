let start = "AAA"
let escape = "ZZZ"

let (|Right|Left|) step =
    match step with
    | 'R' -> Right
    | _ -> Left

type Node =
    { Name: string
      Left: string
      Right: string }

let debug x =
    printfn "%A" x
    x

let debugU x = printfn "%A" x

module Part1 =
    let rec loopOver steps (directions: string) node nodes =
        match directions[steps % directions.Length] with
        | Left ->
            match node.Left = escape with
            | true -> steps + 1
            | _ ->
                let nextNode = nodes |> Seq.find (fun n -> n.Name = node.Left)
                loopOver (steps + 1) directions nextNode nodes
        | Right ->
            match node.Right = escape with
            | true -> steps + 1
            | _ ->
                let nextNode = nodes |> Seq.find (fun n -> n.Name = node.Right)
                loopOver (steps + 1) directions nextNode nodes

let toNode (line: string) =
    let parts = line.Split('=', System.StringSplitOptions.TrimEntries)

    let nodes =
        parts[1]
            .Trim([| '('; ')' |])
            .Split(',', System.StringSplitOptions.TrimEntries)

    { Node.Name = parts[0]
      Left = nodes[0]
      Right = nodes[1] }

let input =
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> System.IO.File.ReadAllLines

let leftRight = input[0]

let nodes = input |> Array.skip 2 |> Array.map toNode

let solve =
    Part1.loopOver 0 leftRight (nodes |> Array.find (fun n -> n.Name = start)) nodes

solve
