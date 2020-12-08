let input = "1113122113"

let transform (s: char list) =
    let sb = new System.Text.StringBuilder()
    let rec l count (last: char) text =
        match text with
        | head :: tail ->
            if head = last
            then l (count + 1) last tail
            else
                sb.Append(count)
                sb.Append(last) 
                l 1 head tail
        | [] -> sb.Append(count)
                sb.Append(last) 
                sb.ToString()

    l 1 s.[0] s.[1..]

let transformN times s =
    let mutable result = s
    for i in 0..times-1 do
        result <- transform (Seq.toList result)
    result

printfn "part1: %d" (transformN 40 input).Length
printfn "part2: %d" (transformN 50 input).Length
