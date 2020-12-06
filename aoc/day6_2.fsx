open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let turnOn (lights:int[,]) (x1, y1) (x2, y2) =
    for x in x1..x2 do
        for y in y1..y2 do
            lights.[x, y] <- lights.[x, y] + 1
    lights

let turnOff (lights:int[,]) (x1, y1) (x2, y2) =
    for x in x1..x2 do
        for y in y1..y2 do
            lights.[x, y] <- max 0 (lights.[x, y] - 1)
    lights

let toggle (lights:int[,]) (x1, y1) (x2, y2) =
    for x in x1..x2 do
        for y in y1..y2 do
            lights.[x, y] <- lights.[x, y] + 2
    lights

let mutable lights =
    Array2D.init 1000 1000 (fun _ _ -> 0)

let tupleToInt (s:string) =
    let ss = s.Split(",")
    (int ss.[0], int ss.[1])

let getCoordinates line =
    match line with
    | Regex @"(\d*,\d*) through (\d*,\d*)" [t1; t2]
        -> Some (tupleToInt t1, tupleToInt t2)
    | _ -> None

let applyLine lights line =
    let (t1, t2) = (getCoordinates line).Value
    if line.StartsWith("turn on")
    then turnOn lights t1 t2
    elif line.StartsWith("turn off")
    then turnOff lights t1 t2
    else toggle lights t1 t2    

let countLitLights (lights:int[,]) =
    let rows = lights.GetLength(0)
    let cols = lights.GetLength(1)
    let mutable total = 0
    for row in 0..rows-1 do
        for col in 0..cols-1 do
            total <- total + lights.[row, col]
    total

File.ReadAllLines "input_6.txt"
|> Seq.fold applyLine lights
|> countLitLights