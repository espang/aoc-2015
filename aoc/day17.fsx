open System.IO

let containers =
    File.ReadAllLines "input_17.txt"
    |> Seq.map int
    |> Seq.toList

let findCombinations volume containers =
    let mutable counter: Map<int, int> = Map.empty

    let incr numberContainers =
        counter <-
            counter.Add
                (numberContainers,
                 match counter.TryFind numberContainers with
                 | Some v -> v + 1
                 | None -> 1)

    let rec loop (acc, n) volume containers =
        match containers with
        | head :: tail ->
            if head > volume then
                loop (acc, n) volume tail
            else
                loop (acc, n + 1) (volume - head) tail
                + (loop (acc, n) volume tail)
        | [] -> if volume = 0
                then
                    incr n
                    1
                else 0

    loop (0, 0) volume containers |> ignore
    counter

findCombinations 150 containers
