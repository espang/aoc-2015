open System.IO

let content = File.ReadAllText "input_3.txt"

let visited content =
    let rec _move acc (x, y) route =
        match route with
        | h :: t ->
            match h with
            | '^' -> _move (Set.add (x, y + 1) acc) (x, y + 1) t
            | 'v' -> _move (Set.add (x, y - 1) acc) (x, y - 1) t
            | '<' -> _move (Set.add (x + 1, y) acc) (x + 1, y) t
            | '>' -> _move (Set.add (x - 1, y) acc) (x - 1, y) t
            | _ -> None
        | [] -> Some acc
    _move (Set.empty.Add(0, 0)) (0, 0) content

let v = visited (Seq.toList content)
Set.count v.Value

Set.contains (0, 0) v.Value

let splitInstructions content =
    let rec _split (real, robo) route forReal =
        match route with
        | h::t -> if forReal 
                  then _split (Seq.append real [h], robo) t false
                  else _split (real, Seq.append robo [h]) t true
        | [] -> (real, robo)
    
    _split ([], []) content true

let (route1, route2) = splitInstructions (Seq.toList content)
let santasHouses = visited (Seq.toList route1)
let roboSantasHouses = visited (Seq.toList route2)

Set.union santasHouses.Value roboSantasHouses.Value |> Set.count