open System.IO

type Parcel =
    { Length: int
      Width: int
      Height: int }

let parseRaw (s: string) =
    let numbers = s.Split("x")
    if numbers.Length = 3 then
        Some
            { Length = int numbers.[0]
              Width = int numbers.[1]
              Height = int numbers.[2] }
    else
        None

let smallestSide parcel =
    min (min (parcel.Length * parcel.Width) (parcel.Width * parcel.Height)) (parcel.Height * parcel.Length)

let surface parcel =
    (2 * parcel.Length * parcel.Width)
    + (2 * parcel.Width * parcel.Height)
    + (2 * parcel.Height * parcel.Length)

let paperFor parcel =
    match parcel with
    | Some p ->
        (surface p)
        + (smallestSide p)
    | None -> 0

File.ReadAllLines "input_2.txt" |> Array.sumBy (parseRaw >> paperFor)

let shortestDistAround parcel =
    let x1 = min parcel.Length parcel.Width
    let x2 = min parcel.Height (max parcel.Length parcel.Width)
    x1 + x1 + x2 + x2

let volume parcel = parcel.Length * parcel.Width * parcel.Height

let ribbonFor parcel =
    match parcel with
    | Some p -> (volume p) + (shortestDistAround p)
    | None -> 0

File.ReadAllLines "input_2.txt" |> Array.sumBy (parseRaw >> ribbonFor)
