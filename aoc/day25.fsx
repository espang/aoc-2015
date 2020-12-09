let sumTo n = n * (n + 1) / 2

let numberOf row col =
    sumTo (row + col - 1)
    - row + 1

let rec multiply n (start:bigint) (factor:bigint) (rem:bigint) =
    if n = 0 
    then start
    else
        let next = start * factor % rem
        if n % 10000 = 0 
        then printfn "number %d" n

        multiply (n-1) next factor rem

// input:
let start = 20151125
let factor = 252533
let remainder = 33554393
let row = 3010
let col = 3019
let index = numberOf row col
// part 1
multiply (index - 1) (bigint start) (bigint factor) (bigint remainder)

