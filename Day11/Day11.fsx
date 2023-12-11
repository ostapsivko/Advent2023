let readMap =
    let input =
        System.IO.File.ReadAllLines(System.IO.Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))

    let map = Array2D.init input.Length input[0].Length (fun x y -> input[x].[y])

    map

let expanse = 100_00_00L //

let isGalaxy cell = cell = '#'

let mapGalaxies map =
    let rec findNextGalaxy y x (map: char array2d) galaxies =
        match y, x with
        | j, i when j + 1 = map.GetLength 0 && i + 1 = map.GetLength 1 ->
            if isGalaxy map[j, i] then
                [ j, i ] @ galaxies
            else
                galaxies
        | j, i when i + 1 = map.GetLength 1 ->
            if isGalaxy map[j, i] then
                findNextGalaxy (j + 1) 0 map [ j, i ] @ galaxies
            else
                findNextGalaxy (j + 1) 0 map galaxies
        | j, i ->
            if isGalaxy map[j, i] then
                findNextGalaxy j (i + 1) map [ j, i ] @ galaxies
            else
                findNextGalaxy j (i + 1) map galaxies

    findNextGalaxy 0 0 map list.Empty |> List.rev

let rec makePairs galaxies =
    let rec createPair galaxies pairs =
        match galaxies with
        | head :: tail ->
            createPair (galaxies |> List.skip 1) ([ head ] |> List.allPairs tail)
            @ pairs
        | [] -> pairs

    createPair galaxies []

let getDistance ((x1, y1), (x2, y2)) =
    let dx = abs (x2 - x1)
    let dy = abs (y2 - y1)
    dx + dy

//assume it's a square
let createAllNumsSet (arr: 'a [,]) =
    let size = arr.GetLongLength 0
    seq { 0L .. size - 1L } |> Set.ofSeq

let findEmptyRows galaxies map =
    let galaxiesSet =
        galaxies
        |> List.map (fun (y, _) -> y)
        |> Set.ofList

    let allNumsSet = createAllNumsSet map

    Set.difference allNumsSet galaxiesSet

let findEmptyCols galaxies map =
    let galaxiesSet =
        galaxies
        |> List.map (fun (_, x) -> x)
        |> Set.ofList

    let allNumsSet = createAllNumsSet map

    Set.difference allNumsSet galaxiesSet

let increaseDistance (galaxies: (int64 * int64) list) emptyRows emptyCols =
    galaxies
    |> List.map (fun (y, x) ->
        let xDiff =
            emptyCols
            |> Set.filter ((>) x)
            |> Set.count
            |> int64

        (y, x + ((expanse - 1L) * xDiff)))
    |> List.map (fun (y, x) ->
        let yDiff =
            emptyRows
            |> Set.filter ((>) y)
            |> Set.count
            |> int64

        (y + ((expanse - 1L) * yDiff), x))

let solve =
    let map = readMap

    let rawGalaxies =
        map
        |> mapGalaxies
        |> List.map (fun (y, x) -> (y |> int64, x |> int64))

    let emptyRows = findEmptyRows rawGalaxies map
    let emptyCols = findEmptyCols rawGalaxies map

    let galaxies = increaseDistance rawGalaxies emptyRows emptyCols

    let pairs = galaxies |> makePairs
    pairs |> List.map getDistance |> List.sum

solve
