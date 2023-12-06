open System

module Part1 =

    type Range =
        { SourceStart: uint64
          DestinationStart: uint64
          RangeLength: uint64 }
        member r.Item key =
            match key with
            | value when
                r.SourceStart <= value
                && value <= r.SourceStart + r.RangeLength
                ->
                let offset = value - r.SourceStart
                r.DestinationStart + offset
            | _ -> key

    type Mapping =
        { From: string
          To: string
          Ranges: Range array }

    let toMapping (lines: string array) =
        // lines |> Array.iter Console.WriteLine

        let toFrom =
            (lines[0].Split(' ').[0].Split('-')[0], lines[0].Split(' ').[0].Split('-')[2])

        let mappings =
            lines
            |> Array.skip 1
            |> Array.map (fun m ->
                let mapArr = m.Split(' ')
                (mapArr[1] |> uint64, mapArr[0] |> uint64, mapArr[2] |> uint64))

        let toFromMaps =
            mappings
            |> Array.map (fun (t, f, r) ->
                { SourceStart = t
                  DestinationStart = f
                  RangeLength = r })

        { To = toFrom |> snd
          From = toFrom |> fst
          Ranges = toFromMaps }

    let rec loop (mapList: Mapping list) (lines: string array) =
        match lines with
        | [||] -> mapList
        | _ ->
            let toMap = lines |> Array.takeWhile (fun l -> l <> "")

            let mapRes = toMap |> toMapping

            let toSkip =
                if lines.Length <= toMap.Length then
                    lines.Length
                else
                    toMap.Length + 1

            loop (mapRes :: mapList) (lines |> Array.skip toSkip)

    let processedMappings inp =
        inp |> Array.skip 2 |> loop List.empty |> List.rev

    let containsKey key range =
        range.SourceStart <= key
        && key <= range.SourceStart + range.RangeLength

    let findLastMapping mappings seed =
        let rec findNext key mapFrom (mappings: Mapping list) =
            match mappings
                  |> List.tryFind (fun m -> m.From = mapFrom)
                with
            | Some map ->
                match map.Ranges |> Array.tryFind (containsKey key) with
                | Some conversion -> findNext (conversion.Item key) map.To mappings
                | _ -> findNext key map.To mappings
            | _ -> key

        findNext seed "seed" mappings

let input =
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> System.IO.File.ReadAllLines

let seeds =
    input[0].[7..]
        .Split(
            ' ',
            StringSplitOptions.TrimEntries
            ||| StringSplitOptions.RemoveEmptyEntries
        )
    |> Array.map uint64

seeds
|> Array.map (Part1.findLastMapping (Part1.processedMappings input))
|> Array.min
|> Console.WriteLine
