open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

type LightState =
    | On
    | Off

let turnOn (lights:LightState[,]) (x1, y1) (x2, y2) =
    for x in x1..x2 do
        for y in y1..y2 do
            lights.[x, y] <- On
    lights

let turnOff (lights:LightState[,]) (x1, y1) (x2, y2) =
    for x in x1..x2 do
        for y in y1..y2 do
            lights.[x, y] <- Off
    lights

let toggle (lights:LightState[,]) (x1, y1) (x2, y2) =
    for x in x1..x2 do
        for y in y1..y2 do
            lights.[x, y] <- match lights.[x, y] with
                             | On -> Off
                             | Off -> On
    lights
    
// turn on 0,0 through 999,999
// toggle 0,0 through 999,0
// turn off 499,499 through 500,500
let mutable lights =
    Array2D.init 1000 1000 (fun _ _ -> Off)

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

let countLitLights (lights:LightState[,]) =
    let rows = lights.GetLength(0)
    let cols = lights.GetLength(1)
    let mutable total = 0
    for row in 0..rows-1 do
        for col in 0..cols-1 do
            if lights.[row, col] = On
            then total <- total + 1
    total

File.ReadAllLines "input_6.txt"
|> Seq.fold applyLine lights
|> countLitLights