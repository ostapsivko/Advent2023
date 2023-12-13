let patterns =
    System.IO.File.ReadAllLines(System.IO.Path.Combine(__SOURCE_DIRECTORY__, "test_input_1.txt"))

let pattern2 =
    patterns
    |> Array.skipWhile ((<>) "")
    |> Array.skip 1

// let rotate (pattern : string array) =
//     let result = Array.init pattern.[0].Length

//     for line in lines do

let patternLength = pattern2 |> Array.length
let distinct = pattern2 |> Array.distinct

distinct
|> Array.map (fun line ->
    pattern2
    |> Array.tryFindIndex (fun patt -> patt = line))
