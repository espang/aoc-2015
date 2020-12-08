open System.IO

let consumeHex s =
    let rec consume text first =
        match text with
        | h::t ->
            if System.Char.IsDigit h  || ('a' <= h && h <= 'f')
            then if first 
                 then consume t false
                 else (true, t)
            else
                (false, s)
        | [] -> (false, s)

    consume s true

let consumeQuoted s =
    printfn "consumeQuoted: '%A'" s
    match s with
    | '\\'::rest -> (true, rest)
    | '\"'::rest -> (true, rest)
    | 'x' ::rest -> consumeHex rest
    | _ -> (false, s)

let consumeStr (s:string) =
    let rec consume acc (text:char list) =
        if text.Length = 1
        then
            match text with
            | '\"'::_ -> acc
            | _ ->  failwith "should end with a quote"
        else
            match text with
            | '\\'::t ->
                match consumeQuoted t with
                | (false, _) -> failwith "unexpected quoted"
                | (true, rest) -> consume (acc + 1) rest
            | h::t ->
                consume (acc + 1) t
            | [] -> failwith "unreachable"

    match (Seq.toList s) with
    | '\"'::tail -> consume 0 tail
    | _ -> failwith "sould start with a quote"

let fline = File.ReadAllLines "input_8.txt" |> Seq.head

let lineToCounts (l:string) =
    (l.Length, consumeStr l)

