let input = System.IO.File.ReadLines (__SOURCE_DIRECTORY__ + "/input/day5.txt") |> Seq.head

let polymerReducer (poly : char seq) = 
    ([], poly) 
    ||> Seq.fold (fun acc c -> 
            match acc with 
            | h :: t when System.Math.Abs (int c - int h) = 32 -> t
            | _                                         -> c :: acc)
    |> List.rev

let polymerReducerIgnore (poly : char seq) (punit : char) = 
    let pdig = int punit
    ([], poly) 
    ||> Seq.fold (fun acc c -> 
            if [32; -32; 0] |> Seq.exists ((=) ((int c) - pdig)) then acc
            else match acc with 
                 | h :: t when System.Math.Abs (int c - int h) = 32 -> t
                 | _                                                -> c :: acc)
    |> List.rev

let part1 = input |> polymerReducer |> Seq.length
let part2 = ['a' .. 'z'] 
            |> Seq.map (polymerReducerIgnore input >> Seq.length)
            |> Seq.min
            