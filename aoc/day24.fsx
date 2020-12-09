open System.IO

let allGroups weight weights =
    let rec group acc w weights =
        if w = weight
        then Set.empty.Add(acc)
        else if w > weight
             then Set.empty
             else
                match weights with
                | head::tail ->
                    Set.union
                        // find all subsets not containing head
                        (group acc w tail)
                        // find all subsets containing head
                        (group (Set.add head acc) (w + head) tail)
                | [] -> Set.empty

    group Set.empty 0 weights

let testGroup (group:Set<int>) (weights:Set<int>) =
    let otherWeights =
        Set.difference weights group
        |> Set.toList
    let totalWeight =
        Set.fold (+) 0 group
    let groups = allGroups totalWeight otherWeights
    
    not (Set.isEmpty groups)

let findFirst groups weights =
    let rec find groups =
        match groups with
        | group::others ->
            if testGroup group weights
            then Some group
            else find others
        | []-> None

    find groups

let weights =
    File.ReadAllLines "input_24.txt"
    |> Seq.map int
    |> Seq.toList

let total = Seq.sum weights
let weightPerGroup = total / 3
let groups =
    allGroups weightPerGroup (Seq.toList weights)

let sortedGroups =
    groups
    |> Set.toList
    |> List.sortBy (fun x -> Set.count x,
                             x |> Set.toList |> List.map bigint |> List.reduce (( * )) )

let best =
    findFirst sortedGroups (Set.ofSeq weights)

best.Value
|> Set.toList
|> List.map bigint
|> List.reduce (*)


best.Value |> Set.toList |> List.sum




// part 2
let total2 = Seq.sum weights
let weight2PerGroup = total / 4

let groups2 =
    allGroups weight2PerGroup (Seq.toList weights)

let sortedGroups2 =
    groups2
    |> Set.toList
    |> List.sortBy (fun x -> Set.count x,
                             x |> Set.toList |> List.map bigint |> List.reduce (( * )) )

sortedGroups2 |> List.head |> Set.toList |> List.reduce (*)

let testGroup2 (group:Set<int>) (weights:Set<int>) =
    let otherWeights =
        Set.difference weights group
    let totalWeight = Set.fold (+) 0 group
    let groups = allGroups totalWeight (Set.toList otherWeights)
    
    let rec any groups =
        match groups with
        | group::tail ->
            testGroup group otherWeights 
        | [] -> false

    any (Set.toList groups)

let findFirst2 groups weights =
    let rec find groups =
        match groups with
        | group::others ->
            if testGroup2 group weights
            then Some group
            else find others
        | []-> None

    find groups

let best2 =
    findFirst2 sortedGroups2 (Set.ofSeq weights)

best2.Value
|> Set.toList
|> List.map bigint
|> List.reduce (*)

sortedGroups2.[..15]
|> List.map (fun x -> x, x |> Set.toList |> List.reduce (*))

67 * 101 * 109 * 113

sortedGroups2
|> List.sortBy (fun x -> x |> Set.toList |> List.map bigint |> List.reduce (*))
|> List.take 10