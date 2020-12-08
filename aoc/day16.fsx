// Sue 500: cars: 1, perfumes: 6, vizslas: 1
open System.IO

let dna =
    Map.empty.Add("children", 3).Add("cats", 7).Add("samoyeds", 2).Add("pomeranians", 3).Add("akitas", 0)
       .Add("vizslas", 0).Add("goldfish", 5).Add("trees", 3).Add("cars", 2).Add("perfumes", 1)

let toMap (s:string) =
    let splitted = s.Split(",")
    splitted
    |> Array.map (fun x -> let _splitted = x.Split(":")
                           (_splitted.[0].Trim(), int _splitted.[1]))
    |> Map.ofArray

let isSubset s subset =
    Map.forall (fun k v -> (Map.containsKey k s) && s.[k] = v) subset


let test pred key (s:Map<string, int>) subset =
    if Map.containsKey key subset
    then
        pred s.[key] subset.[key]
    else true

let testDNA dna subdna =
    test (=) "children" dna subdna
    && test (<) "cats" dna subdna
    && test (=) "samoyeds" dna subdna
    && test (>) "pomeranians" dna subdna
    && test (=) "akitas" dna subdna
    && test (=) "vizslas" dna subdna
    && test (>) "goldfish" dna subdna
    && test (<) "trees" dna subdna
    && test (=) "cars" dna subdna
    && test (=) "perfumes" dna subdna

let parseLine (line:string) =
    let splitted = line.Split(":", 2)
    (splitted.[0],
     toMap splitted.[1])

// part1
File.ReadAllLines "input_16.txt"
|> Seq.map parseLine
|> Seq.filter (fun (_, pdna) -> isSubset dna pdna)

// part2
File.ReadAllLines "input_16.txt"
|> Seq.map parseLine
|> Seq.filter (fun (_, pdna) -> testDNA dna pdna)