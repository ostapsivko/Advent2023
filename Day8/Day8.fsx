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

module Part2 =
    //courtesy of Rosettacode https://rosettacode.org/wiki/Least_common_multiple
    let rec gcd (x: int64) (y: int64) = if y = 0 then abs x else gcd y (x % y)

    let lcm x y = x * y / (gcd x y)

    let toLastNode (directions: string) nodes startNode =
        let mutable steps = 0
        let mutable node = startNode

        while node.Name.EndsWith('Z') |> not do
            match directions[steps % directions.Length] with
            | Left ->
                node <- nodes |> Seq.find (fun n -> n.Name = node.Left)
                steps <- steps + 1
            | Right ->
                node <- nodes |> Seq.find (fun n -> n.Name = node.Right)
                steps <- steps + 1

        steps

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

let solvePart1 =
    Part1.loopOver 0 leftRight (nodes |> Array.find (fun n -> n.Name = start)) nodes

let solvePart2 =
    nodes
    |> Array.filter (fun n -> n.Name.EndsWith('A'))
    |> Array.map (Part2.toLastNode leftRight nodes)
    |> Array.map int64
    |> Array.reduce (fun x y -> Part2.lcm x y)

solvePart1
solvePart2
