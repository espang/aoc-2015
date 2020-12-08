let input = 34_000_000

let factorise (number:int) =
    let limit = int (ceil (sqrt (double number)))
    let factors =
        [1..limit]
        |> List.filter (fun x -> number % x = 0)
    let pairs = 
        List.map (fun x -> number/x) factors
    factors @ pairs

let rec sumOfFactors number limit =
    let s = 10 * (factorise number |> List.sum) 
    if s >= limit
    then number 
    else sumOfFactors (number + 1) limit

// sumOfFactors 1 input

let lessThan50 n x =
    // look at the pair that together is the current house number
    // when a factor is below or equals 50 the other factor is 
    // valid.
    let f = n/x
    match (x, f) with
    | (v1, v2) when v1 = v2 -> v1
    | (v1, v2) when v1 <= 50 && v2 <= 50 -> v1+v2
    | (v1, v2) when v1 <= 50 -> v2
    | (v1, v2) when v2 <= 50 -> v1
    | _ -> 0    

let sumPart2 (number:int) =
    let limit = int (sqrt (double number))
    [1..limit]
    |> List.filter (fun x -> number % x = 0)
    |> List.sumBy (lessThan50 number)

let rec find number limit =
    let s = 11 * sumPart2 number 
    if s >= limit
    then number 
    else find (number + 1) limit

find 1 input
