open System.IO

// idea and formula from
// https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/

type Token =
    | T
    | Open
    | Comma
    | Close

let parseString (line:string) =
    let rec loop tokens text =
        match text with
        | 'R'::'n'::tail -> loop (Open::tokens) tail
        | 'A'::'r'::tail -> loop (Close::tokens) tail
        | 'Y'::tail -> loop (Comma::tokens) tail
        | head::tail ->
            if System.Char.IsUpper head
            then loop (T::tokens) tail
            else loop tokens tail
        | [] -> List.rev tokens
    loop [] (Seq.toList line)

let tokens =
    parseString "ORnPBPMgArCaCaCaSiThCaCaSiThCaCaPBSiRnFArRnFArCaCaSiThCaCaSiThCaCaCaCaCaCaSiRnFYFArSiRnMgArCaSiRnPTiTiBFYPBFArSiRnCaSiRnTiRnFArSiAlArPTiBPTiRnCaSiAlArCaPTiTiBPMgYFArPTiRnFArSiRnCaCaFArRnCaFArCaSiRnSiRnMgArFYCaSiRnMgArCaCaSiThPRnFArPBCaSiRnMgArCaCaSiThCaSiRnTiMgArFArSiThSiThCaCaSiRnMgArCaCaSiRnFArTiBPTiRnCaSiAlArCaPTiRnFArPBPBCaCaSiThCaPBSiThPRnFArSiThCaSiThCaSiThCaPTiBSiRnFYFArCaCaPRnFArPBCaCaPBSiRnTiRnFArCaPRnFArSiRnCaCaCaSiThCaRnCaFArYCaSiRnFArBCaCaCaSiThFArPBFArCaSiRnFArRnCaCaCaFArSiRnFArTiRnPMgArF"

let elements = tokens.Length
let parenthesis = tokens |> List.filter (fun x -> x = Open || x = Close) |> List.length
let commas = tokens |> List.filter (fun x -> x = Comma) |> List.length

let part2 = elements - parenthesis - 2 * commas - 1
