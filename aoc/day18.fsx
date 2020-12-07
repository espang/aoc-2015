open System.IO

type LightState =
    | On
    | Off

let parse line =
    Seq.map (fun x -> if x = '#' then On else Off) line

let lights =
    File.ReadAllLines "input_18.txt"
    |> Seq.map parse
    |> array2D

let neighbours (x, y) rows cols =
    [ (-1, -1)
      (-1, 0)
      (-1, 1)
      (0, -1)
      (0, 1)
      (1, -1)
      (1, 0)
      (1, 1) ]
    |> Seq.map (fun (dx, dy) -> (x + dx, y + dy))
    |> Seq.filter (fun (x, y) -> (x >= 0 && x < cols) && (y >= 0 && y < rows))

let nextStateOf (x, y) (lights: LightState [,]) =
    let numberNeighbours =
        neighbours (x, y) 100 100
        |> Seq.filter (fun (x, y) -> On = lights.[x, y])
        |> Seq.length

    match lights.[x, y] with
    | On ->
        if numberNeighbours = 2 || numberNeighbours = 3
        then On
        else Off
    | Off -> if numberNeighbours = 3 then On else Off

let iterate lights =
    let mutable lights' = Array2D.init 100 100 (fun _ _ -> Off)
    for x in 0..99 do
        for y in 0..99 do
            lights'.[x,y] <-
                match (x, y) with
                | (0, 0)
                | (0, 99)
                | (99, 0)
                | (99, 99) ->
                    On
                | _ ->
                    nextStateOf (x, y) lights
    lights'

let rec simulate n lights =
    if n = 0
    then lights
    else simulate (n - 1) (iterate lights)

let lightsOnAfter n lights =
    let after100 = simulate n lights
    let mutable total = 0
    for x in 0..99 do
        for y in 0..99 do
            if after100.[x,y] = On then total <- total + 1
    total

let mutable lights2 = lights
lights2.[0,0] <- On
lights2.[0,99] <- On
lights2.[99,0] <- On
lights2.[99,99] <- On

lightsOnAfter 100 lights2
