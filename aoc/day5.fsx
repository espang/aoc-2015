open System.IO

let vowels = Set.empty.Add('a').Add('e').Add('i').Add('o').Add('u')

let countVowels s =
    s
    |> Seq.filter (fun x -> Set.contains x vowels)
    |> Seq.length
    
let hasRepeatedLetter (s:char list) =
    let rec _find last s =
        match s with
        | head::tail -> if head = last then true else _find head tail
        | [] -> false
    _find s.[0] s.[1..]

let badStrings = ["ab"; "cd"; "pq"; "xy"]

let containsBadStrings (s:string) =
    Seq.map (fun (x:string) -> s.Contains(x)) badStrings |> Seq.forall not |> not

let isNice s =
    (countVowels (Seq.toList s)) >= 3
    && (hasRepeatedLetter (Seq.toList s))
    && (not (containsBadStrings s))

File.ReadAllLines "input_5.txt"
|> Seq.filter isNice
|> Seq.length

let containsPair (s:string) =
    let rec loop pairs (prev:char) text =
        match text with
        | head::tail ->
            if Set.contains (prev, head) pairs
            then 
                if prev = head
                then 
                    if s.Replace([prev;head] |> System.String.Concat, "").Length <= s.Length-4
                    then true
                    else loop pairs head tail
                else true
            else loop (Set.add (prev, head) pairs) head tail
        | [] -> false
    
    let cs = Seq.toList s
    if cs.Length <= 1
    then false
    else loop Set.empty cs.[0] cs.[1..]

let containsRepeatedLetterWithOneBetween (s:char list) =
    let rec loop pprev prev text =
        match text with
        | head :: tail ->
            if pprev = head
            then true
            else loop prev head tail
        | []-> false
    
    if s.Length <= 2
    then false
    else loop s.[0] s.[1] s.[2..]

let isNice2 s = (containsPair s) && (containsRepeatedLetterWithOneBetween (Seq.toList s))

File.ReadAllLines "input_5.txt"
|> Seq.filter isNice2
|> Seq.length
