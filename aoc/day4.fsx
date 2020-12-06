open System.Security.Cryptography
open System.Text

let input = "yzbqklnj"

let md5 (data : byte array) : string =
    use md5 = MD5.Create()
    (StringBuilder(), md5.ComputeHash(data))
    ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
    |> string

let hash = md5 "hello world"B;

let findLowestNumber (pred: string -> bool) input =
    let rec _next number =
        let hash = md5 (Encoding.ASCII.GetBytes(input + (string number)))
        let test = pred hash
        match test with
        | true -> number
        | false -> _next (number + 1)
    
    _next 1

findLowestNumber (fun x -> x.StartsWith "00000") input
//part 2
findLowestNumber (fun x -> x.StartsWith "000000") input