open System.IO

let toTriple (l: string) =
    let split1 = l.Split("=")
    let split2 = split1.[0].Split("to")
    (split2.[0].Trim(), split2.[1].Trim(), int (split1.[1].Trim()))

let triples =
    File.ReadAllLines "input_9.txt"
    |> Seq.map toTriple

let towns =
    Seq.fold (fun (acc: Set<string>) (t1, t2, _) -> acc.Add(t1).Add(t2)) Set.empty triples
    |> Set.toList |> List.sort

let mutable distances = Array2D.init towns.Length towns.Length (fun _ _ -> 0)

for (t1, t2, dist) in triples do
    let i1 = List.findIndex (fun x -> x = t1) towns
    let i2 = List.findIndex (fun x -> x = t2) towns
    distances.[i1, i2] <- dist
    distances.[i2, i1] <- dist

let exhaustiveSearch (towns: string list) (distances:int[,]) =
    let mutable totalDistances = Set.empty
    let rec loop distance pos visited =
        if (Set.count visited) = towns.Length
        then
            totalDistances <- Set.add distance totalDistances
        else 
            for i in 0..towns.Length-1 do
                if not (Set.contains i visited)
                then 
                    loop (distance + distances.[pos, i]) i (Set.add i visited)

    for i in 0..towns.Length-1 do
        loop 0 i (Set.empty.Add(i))

    totalDistances
 
let pathDistances = exhaustiveSearch towns distances

Set.minElement pathDistances
Set.maxElement pathDistances