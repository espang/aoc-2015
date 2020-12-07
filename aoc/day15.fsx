// input
let ingredients = [
    [4; -2; 0; 0; 5]
    [0; 5; -1; 0; 8]
    [-1; 0; 5; 0; 6]
    [0; 0; -2; 2; 1]
]

let score (ingredients: int list list)
          (amounts: int []) =
    let mutable score = Array.zeroCreate 4
    let mutable calories = 0
    
    for idx in 0..3 do
        let ingredient = ingredients.[idx]
        for prop in 0..3 do
            score.[prop] <- score.[prop] + ingredient.[prop] * amounts.[idx]
        calories <- calories + amounts.[idx] * ingredient.[4]
 
    if calories = 500 && Array.forall (fun x -> x > 0) score 
    then Array.fold (fun acc v -> acc * (uint64 v)) (uint64 1) score
    else uint64 0

let values =
    seq {
        for i1 in 0..100 do
            for i2 in 0..100 do
                for i3 in 0..100 do
                    for i4 in 0..100 ->
                        if i1 + i2 + i3 + i4 = 100
                        then score ingredients [|i1;i2;i3;i4|]
                        else uint64 0
    }

Seq.max values