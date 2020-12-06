let input = "hepxcrrq"

let incrInput s =
    let arr = Seq.toArray s
    let rec loop idx =
        if arr.[idx] = 'z'
        then
            arr.[idx] <- 'a'
            loop (idx - 1) 
        else
            arr.[idx] <- arr.[idx] + (char 1)
    
    loop 7
    System.String arr    

let validChars s =
    let badLetters = Set.empty.Add('i').Add('o').Add('l')
    let pred = (fun x -> (System.Char.IsLetter x) && (not (Set.contains x badLetters)))
    Seq.forall pred s

let hasTwoPairs s =
    let rec loop acc last ok s =
        match s with
        | head::tail -> if ok && head = last
                        then if acc = 1
                             then true
                             else loop 1 head false tail
                        else loop acc head true tail
        | [] -> false
    let cs = Seq.toList s
    loop 0 cs.[0] true cs.[1..]

let hasThreeIncLetters s =
    let rec loop acc last s=
        match s with
        | head::tail ->
            if head = (char (last + 1))
            then if acc = 2
                 then true
                 else loop 2 (int head) tail
            else loop 1 (int head) tail
        | [] -> false

    let cs = Seq.toList s
    loop 1 (int cs.[0]) cs.[1..] 

let test s =    
    hasThreeIncLetters s
    && hasTwoPairs s
    && validChars s

let nextPassword last =
    let rec loop s =
        if s = "zzzzzzzz"
        then None
        else
            let n = incrInput s
            if test n
            then Some n
            else loop n

    loop last

nextPassword input

nextPassword "hepxxyzz"