open System.IO

let testTransformation =
    Map.empty.Add([ 'H' ], [ "HO"; "OH" ]).Add([ 'O' ], [ "HH" ])

let listToString (cl: char list) = System.String(List.toArray cl)

let combineAll (prefix: char list) s (postfix: char list) =
    prefix @ (Seq.toList s) @ postfix |> listToString

let setFrom key (transformation: Map<char list, string list>) prefix postfix =
    if transformation.ContainsKey key then
        transformation.[key]
        |> List.map (fun s -> combineAll prefix s postfix)
        |> Set.ofList
    else
        Set.empty

let isPrefix (list:char list) (sublist:char list) =
    if list.Length >= sublist.Length
       && list.[..sublist.Length-1] = sublist
    then
        true, list.[sublist.Length..]
    else
        false, list

let transform (transformation: Map<char list, string list>) text =
    let mutable molecules = Set.empty
    let rec loop prefix text =
        match text with
        | h::t ->
            for itm in transformation do
                match isPrefix text itm.Key with
                | (true, tail) -> 
                    molecules <- Set.union molecules (setFrom itm.Key transformation prefix tail)
                | (false, _) -> ()
            loop (List.append prefix [h]) t
        | [] -> ()
    loop [] (Seq.toList text)
    molecules

// transform testTransformation "HOH"
// transform testTransformation "HOHOHO"

let input =
    "ORnPBPMgArCaCaCaSiThCaCaSiThCaCaPBSiRnFArRnFArCaCaSiThCaCaSiThCaCaCaCaCaCaSiRnFYFArSiRnMgArCaSiRnPTiTiBFYPBFArSiRnCaSiRnTiRnFArSiAlArPTiBPTiRnCaSiAlArCaPTiTiBPMgYFArPTiRnFArSiRnCaCaFArRnCaFArCaSiRnSiRnMgArFYCaSiRnMgArCaCaSiThPRnFArPBCaSiRnMgArCaCaSiThCaSiRnTiMgArFArSiThSiThCaCaSiRnMgArCaCaSiRnFArTiBPTiRnCaSiAlArCaPTiRnFArPBPBCaCaSiThCaPBSiThPRnFArSiThCaSiThCaSiThCaPTiBSiRnFYFArCaCaPRnFArPBCaCaPBSiRnTiRnFArCaPRnFArSiRnCaCaCaSiThCaRnCaFArYCaSiRnFArBCaCaCaSiThFArPBFArCaSiRnFArRnCaCaCaFArSiRnFArTiRnPMgArF"

let extend (m: Map<char list, string list>) (line: string) =
    let splitted = line.Split("=>")
    let key = (Seq.toList (splitted.[0].Trim()))
    let v = splitted.[1].Trim()

    if m.ContainsKey key then m.Add(key, v :: m.[key]) else m.Add(key, [ v ])

let transformation =
    Seq.fold extend Map.empty (File.ReadAllLines "input_19.txt")

transform transformation input |> Set.count

let invert (transformation:Map<char list, string list>) =
    let mutable m = Map.empty
    for itm in transformation do
        let v = listToString itm.Key
        for newKey in itm.Value do
            let key = Seq.toList newKey
            m <- if m.ContainsKey key then m.Add(key, v :: m.[key]) else m.Add(key, [ v ])
    m
    
let searchBackwards (medicine: string) transformation (start: string) =
    let mutable currentMinimum = System.Int32.MaxValue
    let mutable seen = Map.empty
    let invertedTransformation = invert transformation

    let rec loop acc (input: string) =
        if acc > currentMinimum then
            ()
        else if input = start then
            printfn "found one: %d" acc
            currentMinimum <- acc
            seen <- seen.Add(input, acc)
        else
            if seen.ContainsKey input
            then 
                if seen.[input] <= acc
                then ()
                else
                    seen <- seen.Add(input, acc)
                    let next = transform invertedTransformation input
                    for itm in next do
                        loop (acc + 1) itm
            else
                seen <- seen.Add(input, acc)
                let next = transform invertedTransformation input
                for itm in next do
                    loop (acc + 1) itm
    
    loop 0 medicine
    currentMinimum

// let seenWhen = searchBackwards input inverseTransformation "e"
// printfn "part2 is %d" seenWhen

searchBackwards "HOH" testTransformation "e"
searchBackwards input transformation "e"