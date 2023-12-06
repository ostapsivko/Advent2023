open System

let fileName = "input.txt"

let path = System.IO.Path.Combine(__SOURCE_DIRECTORY__, fileName)

type Mapping =
    { From : string
      To : string
      Conversions: Map<int64, int64> array }

let input = System.IO.File.ReadAllLines(path)

let seeds = 
    input[0].[7..].Split(' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int64

let toMapping (lines : string array) =
    //lines |> Array.iter Console.WriteLine
    
    let toFrom = 
        (lines[0].Split(' ').[0].Split('-')[0], 
        lines[0].Split(' ').[0].Split('-')[2])

    let mappings =
        lines
        |> Array.skip 1
        |> Array.map (fun m -> 
            let mapArr = m.Split(' ')
            (mapArr[1] |> int64, mapArr[0] |> int64, mapArr[2] |> int64))

    let toFromMaps =
        mappings
        |> Array.map (fun (t, f, r)  -> 
            //Console.WriteLine(r)
            (Seq.init (r |> int) (fun x -> ((x |> int64) + t, (x |> int64) + f))) |> Map.ofSeq)

    { To = toFrom |> snd; From = toFrom |> fst; Conversions = toFromMaps }

let rec loop (mapList : Mapping list) (lines : string array) = 
    // lines |> Array.iter Console.WriteLine
    // Console.WriteLine("========")
    match lines with
    | [||] -> mapList
    |  _ -> 
        let toMap =
            lines 
            |> Array.takeWhile (fun l -> l = "" |> not)
        
        let mapRes =
            toMap
            |> toMapping

        // Console.WriteLine(mapList.Length)
        mapList |> List.iter (fun m -> Console.WriteLine(m.ToString()))
        let toSkip = 
            if lines.Length <= toMap.Length then lines.Length
            else toMap.Length + 1  

        loop (mapRes :: mapList) (lines |> Array.skip toSkip)

let processedMappings =
    input
    |> Array.skip 2
    |> loop List.empty 
    //|> List.iter Console.WriteLine
    |> List.rev

let findLastMapping mappings seed =
    let rec findNext key mapFrom (mappings : Mapping list) =
        //Console.WriteLine($"key:{key} from:{mapFrom} mappings:{mappings}")
        
        match mappings |> List.tryFind (fun m -> m.From = mapFrom) with
        | Some map -> 
                match map.Conversions |> Array.tryFind (fun c -> c.ContainsKey key) with
                | Some conversion -> findNext (conversion.Item key) map.To mappings
                | _ -> findNext key map.To mappings
        | _ -> key

    findNext seed "seed" mappings

input
|> Array.iter Console.WriteLine

// seeds
// |> Array.map (findLastMapping processedMappings)
// |> Array.min
// |> Console.WriteLine