open System.IO

type Computer = { A: uint; B: uint }

type Register =
    | A
    | B

type Instruction =
    | Half of Register
    | Triple of Register
    | Increment of Register
    | Jump of int
    | JumpIfEven of Register * int
    | JumpIfOne of Register * int

let apply reg f c =
    match reg with
    | A -> { c with A = f c.A }
    | B -> { c with B = f c.B }

let half reg c = apply reg (fun v -> v / (uint 2)) c

let triple reg c = apply reg (fun v -> (uint 3) * v) c

let increment reg c = apply reg (fun v -> v + (uint 1)) c

let read reg c = 
    match reg with
    | A -> c.A
    | B -> c.B

let execute (instructions:Instruction []) computer =
    let rec executeUntilDone index c =
        if index >= instructions.Length
        then c
        else
            match instructions.[index] with
            | Jump x -> executeUntilDone (index + x) c
            | JumpIfOne (r, x) ->
                executeUntilDone (index + (if (read r c) = (uint 1) then x else 1)) c
            | JumpIfEven (r, x) ->
                executeUntilDone (index + (if (read r c) % (uint 2) = (uint 0) then x else 1)) c
            | Half r ->
                executeUntilDone (index + 1) (half r c)
            | Triple r ->
                executeUntilDone (index + 1) (triple r c)
            | Increment r ->
                executeUntilDone (index + 1) (increment  r c)
    executeUntilDone 0 computer

let registerFrom s =
    match s with
    | "a"
    | "a," -> A
    | "b"
    | "b," -> B
    | _ -> failwith "wrong register"

let parseLine (line: string) =
    let items = line.Split()
    match items.[0] with
    | "inc" -> Increment(registerFrom items.[1])
    | "hlf" -> Half(registerFrom items.[1])
    | "tpl" -> Triple(registerFrom items.[1])
    | "jmp" -> Jump(int items.[1])
    | "jie" -> JumpIfEven(registerFrom items.[1], int items.[2])
    | "jio" -> JumpIfOne(registerFrom items.[1], int items.[2])
    | _ -> failwith (sprintf "no '%s'" items.[0])

let instructions =
    File.ReadAllLines "input_23.txt"
    |> Seq.map parseLine
    |> Seq.toArray

// part 1
execute instructions {A=(uint 0);B=(uint 0)}
// part 2
execute instructions {A=(uint 1);B=(uint 0)}
