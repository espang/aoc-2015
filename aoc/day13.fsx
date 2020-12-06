open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let parse line =
    match line with
    | Regex @"(\w*) would (gain|lose) (\d*) happiness units by sitting next to (\w*)." [ name1; dir; hap; name2 ] ->
        match dir with
        | "gain" -> (name1, name2, int hap)
        | "lose" -> (name1, name2, -(int hap))
        | _ -> failwith "unexpected direction"
    | _ -> failwith "invalid line"

let input =
    File.ReadAllLines "input_13.txt" |> Seq.map parse

let names =
    Seq.fold (fun (acc: Set<string>) (n1, n2, _) -> acc.Add(n1).Add(n2)) Set.empty input

let happiness =
    Seq.map (fun (n1, n2, happy) -> (n1, n2), happy) input
    |> Map.ofSeq

let happinessOf (names: string list) (happinessBetween: string -> string -> int) =
    let first = names.[0]

    let rec loop acc last names =
        match names with
        | head :: tail ->
            let acc' =
                (happinessBetween last head)
                + (happinessBetween head last)
                + acc

            loop acc' head tail
        | [] ->
            (happinessBetween last first)
            + (happinessBetween first last)
            + acc

    loop 0 names.[0] names.[1..]

let permute l =
    let rec loop acc coll =
        match coll with
        | head :: tail ->
            (acc :: coll)
            :: (loop acc tail |> List.map (fun xs' -> head :: xs'))
        | [] -> [ [ acc ] ]

    List.fold (fun acc x -> List.collect (loop x) acc) [ [] ] l

let happinessOrZero happiness left right =
    if Map.containsKey (left, right) happiness then happiness.[(left, right)] else 0

permute (Set.toList names)
|> List.map (fun x -> happinessOf x (happinessOrZero happiness))
|> List.max

let names' = names.Add("myself")

permute (Set.toList names')
|> List.map (fun x -> happinessOf x (happinessOrZero happiness))
|> List.max
