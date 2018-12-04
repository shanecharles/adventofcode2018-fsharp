let input = System.IO.File.ReadLines (__SOURCE_DIRECTORY__ + "/input/day1.txt")
            |> Seq.map System.Int32.Parse
            |> Seq.toArray
let part1 = Seq.reduce (+) input

let part2 = 
        let hash = System.Collections.Generic.HashSet<int> ()
        input |> Seq.unfold (fun s -> Some (s, s))
        |> Seq.collect id
        |> Seq.scan (+) 0
        |> Seq.filter (hash.Add >> not)
        |> Seq.head