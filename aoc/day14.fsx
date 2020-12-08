type Reindeer =
    { Name: string
      Speed: int
      MoveDuration: int
      RestDuration: int }

let reindeers =
    [ { Name = "Vixen"
        Speed = 8
        MoveDuration = 8
        RestDuration = 53 }
      { Name = "Blitzen"
        Speed = 13
        MoveDuration = 4
        RestDuration = 49 }
      { Name = "Rudoplh"
        Speed = 20
        MoveDuration = 7
        RestDuration = 132 }
      { Name = "Cupid"
        Speed = 12
        MoveDuration = 4
        RestDuration = 43 }
      { Name = "Donner"
        Speed = 9
        MoveDuration = 5
        RestDuration = 38 }
      { Name = "Dasher"
        Speed = 10
        MoveDuration = 4
        RestDuration = 37 }
      { Name = "Comet"
        Speed = 3
        MoveDuration = 37
        RestDuration = 76 }
      { Name = "Prancer"
        Speed = 9
        MoveDuration = 12
        RestDuration = 97 }
      { Name = "Dancer"
        Speed = 37
        MoveDuration = 1
        RestDuration = 36 } ]

let distanceAfter dur reindeer =
    let rec loop distance timeLeft moving =
        match moving with
        | true ->
            if reindeer.MoveDuration <= timeLeft
            then loop (distance + reindeer.Speed * reindeer.MoveDuration) (timeLeft - reindeer.MoveDuration) false
            else distance + reindeer.Speed * timeLeft

        | false ->
            if reindeer.RestDuration <= timeLeft
            then loop distance (timeLeft - reindeer.RestDuration) true
            else distance

    loop 0 dur true

reindeers
|> List.map (distanceAfter 2503)
|> List.max

let inefficientCount dur =
    let mutable count = Map.empty
    let incr name =
        let c = if Map.containsKey name count
                        then count.[name] + 1
                        else 1
        count <- count.Add(name, c)
    for t in 1..dur do
        let leader = List.maxBy (distanceAfter t) reindeers
        incr leader.Name
    count

inefficientCount 2503