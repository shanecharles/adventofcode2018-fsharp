let sample = "#1 @ 146,196: 19x14"

let input = System.IO.File.ReadLines (__SOURCE_DIRECTORY__ + "/input/day3.txt")
            |> Seq.filter (System.String.IsNullOrEmpty >> not)
            |> Seq.toArray

type Claim = { ClaimId: string; Left: int; Top: int; Width: int; Height: int }
let parseClaim (line: string) = 
    let data = line.Replace(":", "").Split([|' '|])
    let lt = data.[2].Split([|','|]) |> Array.map System.Int32.Parse
    let wh = data.[3].Split([|'x'|]) |> Array.map System.Int32.Parse
    { ClaimId=data.[0]; Left=lt.[0]; Top=lt.[1]; Width=wh.[0]; Height=wh.[1] }

let calculateCells claim =
    [ for i in (claim.Left + 1) .. (claim.Left + claim.Width) do 
      for j in (claim.Top + 1) .. (claim.Top  + claim.Height) do 
       yield i,j ]

let part1 = 
    input |> Seq.map parseClaim 
    |> Seq.collect calculateCells
    |> Seq.groupBy id
    |> Seq.filter (snd >> Seq.length >> ((<)1))
    |> Seq.length

let part2 = 
    input |> Seq.map parseClaim
    |> Seq.collect (fun claim -> claim |> calculateCells |> Seq.map (fun cel -> cel, claim.ClaimId))
    |> Seq.groupBy fst
    |> Seq.filter (snd >> Seq.length >> ((<)1))
    |> Seq.map (snd >> snd)
    |> Set.ofSeq
