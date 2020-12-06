open System.IO

let content = File.ReadAllText "input_1.txt"

let goUpAndDown s =
    let rec _loop (floor, step) s =
        if floor = -1 then
            Some step
        else
            match s with
            | h :: t ->
                match h with
                | '(' -> _loop (floor + 1, step + 1) t
                | ')' -> _loop (floor - 1, step + 1) t
                | _ -> None
            | [] -> None

    _loop (0, 0) s

// part 2
goUpAndDown (Seq.toList content)
