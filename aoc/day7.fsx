open System.IO

type Wire =
    | Value of uint16
    | Instruction of (Map<string, Wire> -> uint16 option)

let binary op lhs rhs =
    match lhs with
    | Some (Value l) ->
        match rhs with
        | Some (Value r) -> Some(op l r)
        | _ -> None
    | _ -> None

let unary op value =
    match value with
    | Some (Value v) -> Some(op v)
    | _ -> None

let _ident (m: Map<string, Wire>) l =
    let w = m.TryFind l
    match w with
    | Some (Value v) -> Some v
    | _ -> None

let tryFind (m:Map<string, Wire>) value =
    try
        Some (Value (uint16 value))
    with :? System.FormatException -> m.TryFind value
     
let _and (m: Map<string, Wire>) l r =
    binary (fun x y -> x &&& y) (tryFind m l) (tryFind m r)

let _or (m: Map<string, Wire>) l r =
    binary (fun x y -> x ||| y) (tryFind m l) (tryFind m r)

let _lshift (m: Map<string, Wire>) v amount =
    unary (fun x -> x <<< amount) (tryFind m v)

let _rshift (m: Map<string, Wire>) v amount =
    unary (fun x -> x >>> amount) (tryFind m v)

let _not (m: Map<string, Wire>) v = unary (fun x -> ~~~x) (tryFind m v)

let opToFun op lhs rhs =
    match op with
    | "AND" -> (fun m -> _and m lhs rhs)
    | "OR" -> (fun m -> _or m lhs rhs)
    | "LSHIFT" -> (fun m -> _lshift m lhs (int rhs))
    | "RSHIFT" -> (fun m -> _rshift m lhs (int rhs))
    | _ -> failwith (sprintf "unexpected operation %s" op)

let parseLHS (lhs: string) =
    let splitted = lhs.Trim().Split()
    let l = splitted.Length
    printfn "splitted: %A" splitted
    match l with
    | 1 ->
        try
            Value(uint16 (int splitted.[0]))
        with :? System.FormatException -> Instruction(fun m -> _ident m splitted.[0])
    | 2 -> Instruction(fun m -> _not m splitted.[1])
    | 3 -> Instruction(opToFun splitted.[1] splitted.[0] splitted.[2])
    | _ -> failwith "unexpected operation"

let parse (line: string): (string * Wire) =
    let splitted = line.Split("->")
    (splitted.[1].Trim(), parseLHS splitted.[0])

let applyInstruction w m =
    match w with
    | Instruction f -> f m
    | _ -> None

let rec solve find (values: Map<string, Wire>) (instructions: Map<string, Wire>) =
    printfn "start searching! (%d;%d)" values.Count instructions.Count
    if values.ContainsKey find then
        values.Item find
    else
        let mutable values' = values
        let mutable instructions' = instructions
        for itm in instructions do
            let k = itm.Key
            let v = itm.Value
            match (applyInstruction v values) with
            | Some v ->
                printfn "found new value for %s" k
                values' <- values'.Add(k, Value v);
                instructions' <- instructions'.Remove k 
            | _ -> ()
        
        if instructions.Count = instructions'.Count
        then failwith (sprintf "didn't find a new value! %A" values')
        else solve find values' instructions'

let (known, unknown) =
    File.ReadAllLines "input_7.txt"
    |> Seq.map parse
    |> Map.ofSeq
    |> Map.partition (fun k v ->
        match v with
        | Value _ -> true
        | _ -> false)

// part 1
solve "a" known unknown
// part 2
solve "a" (known.Add("b", Value (uint16 956))) unknown

