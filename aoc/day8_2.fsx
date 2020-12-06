open System.IO

let consumeHex s =
    let rec consume text first =
        match text with
        | h::t ->
            if System.Char.IsDigit h  || ('a' <= h && h <= 'f')
            then if first 
                 then consume t false
                 else (true, 1, t)
            else
                (false, 0, s)
        | [] -> (false, 0, s)

    consume s true


let consumeQuoted s =
    match s with
    | '\\'::rest -> (true, 2, rest)
    | '\"'::rest -> (true, 2, rest)
    | 'x' ::rest -> consumeHex rest
    | _ -> (false, 0, s)

let consumeStr s =
    let rec consume acc (text:char list) =
        if text.Length = 1
        then
            match s with
            | '\"'::_ -> (acc + 1)
            | _ ->  failwith "should end with a quote"
        else
            match text with
            | '\\'::t ->
                match consumeQuoted t with
                | (false, _,  _) -> failwith "unexpected quoted"
                | (true, i, rest) -> consume (acc + i) rest
            | h::t ->
                consume acc t
            | [] -> failwith "unreachable"

    match s with
    | '\"'::tail -> consume 1 tail
    | _ -> failwith "sould start with a quote"

let lineToCounts (l:string) =
    (l.Length, 2 + l.Length + consumeStr (Seq.toList l))

File.ReadAllLines "input_8.txt"
|> Seq.sumBy (lineToCounts >> (fun (x, y) -> (y - x)))
