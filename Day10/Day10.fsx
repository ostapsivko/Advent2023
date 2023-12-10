let (|NorthSouth|EastWest|NorthEast|NorthWest|SouthWest|SouthEast|Ground|) =
    function
    | '|' -> NorthSouth
    | '-' -> EastWest
    | 'L' -> NorthEast
    | 'J' -> NorthWest
    | '7' -> SouthWest
    | 'F' -> SouthEast
    | _ -> Ground

let start = 'S'

let input =
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> System.IO.File.ReadAllLines

let numRows, numColumns = (Array.length input), (String.length input[0])

let pipeMap =
    let pipes = Array2D.create numRows numColumns ' '

    for i in 0 .. numRows - 1 do
        for j in 0 .. numColumns - 1 do
            pipes[i, j] <- input[i].[j]

    pipes

let startChar =
    let startLineIdx =
        input
        |> Array.tryFindIndex (fun line -> line.IndexOf(start) <> -1)

    match startLineIdx with
    | Some idx -> (idx, input[ idx ].IndexOf(start))
    | None -> (-1, -1)

let isWithinBounds row col =
    row >= 0
    && row < numRows
    && col >= 0
    && col < numColumns

let debugU x = printfn "%A" x

let debug x =
    printfn "%A" x
    x

let findNextCells x y (pipes: char array2d) cells =
    //printfn $"x {x} y {y}"

    cells
    |> Seq.filter (fun (i, j) -> isWithinBounds i j)
    |> Seq.map (fun (i, j) ->
        match pipes[i, j] with
        | NorthSouth when y = j -> (i, j, true) //((i, j, true, nextPipe) :: path)
        | EastWest when x = i -> (i, j, true) //((i, j, true, nextPipe) :: path)
        | NorthEast when x = i || y = j -> (i, j, true) //((i, j, true, nextPipe) :: path)
        | NorthWest when x = i || y = j -> (i, j, true) //((i, j, true, nextPipe) :: path)
        | SouthWest when x = i || y = j -> (i, j, true) //((i, j, true, nextPipe) :: path)
        | SouthEast when x = i || y = j -> (i, j, true) //((i, j, true, nextPipe) :: path)
        | p when p = start -> (i, j, false) //path //end condition, we found the start tile
        | _ -> (i, j, false))
    |> Seq.filter (fun (_, _, b) -> b)
    |> Seq.map (fun (i, j, _) -> (i, j))
    |> List.ofSeq
//|> debug

let traverse (startX, startY) (allPipes: char array2d) =
    let visitedPath = Array2D.init numRows numColumns (fun _ _ -> false)
    let mutable cycleFound = false

    let rec dfs (x, y) path =
        //printfn $"dfs {x} {y}"
        //printfn $"dfs path {path}"
        //printfn $"visitedPath[x, y] x {x} y {y} {visitedPath[x, y]}"

        let isInPath =
            path
            |> List.tryFind (fun (i, j) -> i = x && j = y)

        if isWithinBounds x y |> not || visitedPath[x, y] then
            //printf "visited path "
            //visitedPath |> debug |> ignore
            //printfn $"isWithinBounds x {x} y {y} {isWithinBounds x y}"
            []
        elif cycleFound then
            printf "visited path "
            visitedPath |> debug |> ignore
            []
        elif x = startX && y = startY && path <> [] then
            path @ [ (x, y) ]
        else
            visitedPath[x, y] <- true
            // printf "visited path "
            // visitedPath |> debug |> ignore

            //printf "next cells "

            let nextCells =
                findNextCells
                    x
                    y
                    allPipes
                    [ (x - 1, y)
                      (x + 1, y)
                      (x, y - 1)
                      (x, y + 1) ]

            // nextCells |> debug |> ignore

            //printf "new path "
            let newPath = path @ [ (x, y) ] //|> debug

            let result =
                List.fold
                    (fun acc nextCell ->
                        if acc <> [] then
                            //printfn "acc: %A" acc
                            acc
                        else
                            //printfn $"checking subPath x {nextCell |> fst} y {nextCell |> snd}"
                            let subPath = dfs nextCell newPath

                            //printfn $"subPath {subPath}"

                            if subPath <> [] then cycleFound <- true
                            //printfn "====cycle found===="
                            //else
                            //printfn $"cycle not found x {nextCell |> fst} y {nextCell |> snd}"

                            subPath)
                    []
                    nextCells

            //printf "result "
            result //|> debug

    // let startCells =
    //     findNextCells
    //         startX
    //         startY
    //         allPipes
    //         [ (startX - 1, startY)
    //           (startX + 1, startY)
    //           (startX, startY - 1)
    //           (startX, startY + 1) ]

    dfs (startX, startY) []

let solveP1 = traverse startChar pipeMap //|> List.length |> (/) 2

solveP1
