let input = System.IO.File.ReadLines (__SOURCE_DIRECTORY__ + "/input/day2.txt")
            |> Seq.toArray

let characterGroups boxId = 
    let groups = boxId |> Seq.groupBy id 
                 |> Seq.map (fun (_,vs) -> Seq.length vs)
                 |> Seq.toArray

    match groups |> Seq.contains 2, groups |> Seq.contains 3 with
    | true, true -> (1,1)
    | true, _    -> (1,0)
    | _, true    -> (0,1)
    | _          -> (0,0)
                        

let part1 = input |> Array.map characterGroups
            |> Array.reduce (fun (tos, ths) (tos', ths') -> tos+tos', ths+ths')
            |> (fun (tos, ths) -> tos * ths)

let part2 = 
    let blankPosition idx (line : string)  = 
        let sqs = Seq.filter (fun i -> i <> idx) [0 .. line.Length - 1]
        System.String (seq { for i in sqs do yield line.[i] } |> Seq.toArray)

    [ 0 .. input.[0].Length - 1]
    |> Seq.collect (fun idx -> 
        input |> Array.map (blankPosition idx)
        |> Seq.groupBy id 
        |> Seq.filter (snd >> Seq.length >> ((<) 1)))
    |> Seq.map fst
    |> Seq.head