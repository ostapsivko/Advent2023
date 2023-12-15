let initSequence =
    System
        .IO
        .File
        .ReadAllText(System.IO.Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))
        .Split(',')

let calcHash (s: string) =
    s
    |> Seq.fold (fun acc curr -> (acc + (curr |> int)) * 17 % 256) 0

let solve = initSequence |> Array.map calcHash |> Array.sum
