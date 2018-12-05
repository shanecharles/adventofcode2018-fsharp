let input = System.IO.File.ReadLines (__SOURCE_DIRECTORY__ + "/input/day4.txt")
            |> Seq.toArray

open System

type Event = { TimeStamp: DateTime;  Action: string; }

let parseLine (line: string) =
    let ts = line.Substring(1,16)
    {TimeStamp=DateTime.Parse(ts); Action=line.Substring(19)}

let groupByGuard actions =
    actions 
    |> Seq.sort
    |> Seq.fold (fun acc act -> 
        match acc, act.Action with
        | _, a when a.StartsWith("Guard") -> (Int32.Parse (a.Replace("#","").Split([|' '|])).[1],[]) :: acc
        | (a,es) :: t, _ -> (a, act :: es) :: t         
    ) []
    |> Seq.map (fun (g, acts) -> g, acts |> Seq.chunkBySize 2 |> Seq.map (fun [|w; s|] -> s.TimeStamp.Minute, int (w.TimeStamp - s.TimeStamp).TotalMinutes))
    |> Seq.groupBy fst |> Seq.map (fun (g, gs) -> g, gs |> Seq.collect snd)
    

let inline getSleepRange (s,r) = [ for i in 0 .. r - 2 -> i + s]

let calculateGuardMinute guardShifts =
    let (guard, times) = guardShifts |> Seq.maxBy (snd >> Seq.sumBy snd)
    let commonMinute = times |> Seq.collect getSleepRange
                        |> Seq.groupBy id
                        |> Seq.maxBy (snd >> Seq.length)
                        |> fst

    guard, commonMinute

let part1 = input |> Array.map parseLine 
            |> groupByGuard 
            |> calculateGuardMinute
            |> (fun (g,m) -> g * m)

let part2 = input |> Array.map parseLine
            |> groupByGuard
            |> Seq.filter (snd >> Seq.isEmpty >> not)
            |> Seq.map (fun (g, gs) -> gs |> Seq.collect getSleepRange |> Seq.groupBy id |> Seq.map (fun (g,ms) -> g, ms |> Seq.length) |> Seq.maxBy snd
                                       |> (fun (m,freq) -> (g, m, freq)))
            |> Seq.maxBy (fun (_,_,freq) -> freq)
            |> (fun (g,m,_) -> g * m)
            
            