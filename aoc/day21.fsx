type Character =
    { Damage: int
      Armor: int
      HitPoints: int }

type Item = { Cost: int; Damage: int; Armor: int }

let applyItem (c:Character) i =
    {c with Damage=c.Damage+i.Damage;
            Armor=c.Armor+i.Armor}

let weapons =
    [ { Cost = 8; Damage = 4; Armor = 0 }
      { Cost = 10; Damage = 5; Armor = 0 }
      { Cost = 25; Damage = 6; Armor = 0 }
      { Cost = 40; Damage = 7; Armor = 0 }
      { Cost = 74; Damage = 8; Armor = 0 } ]

let armors =
    [ { Cost = 13; Damage = 0; Armor = 1 }
      { Cost = 31; Damage = 0; Armor = 2 }
      { Cost = 53; Damage = 0; Armor = 3 }
      { Cost = 75; Damage = 0; Armor = 4 }
      { Cost = 102; Damage = 0; Armor = 5 } ]

let rings =
    [ { Cost = 25; Damage = 1; Armor = 0 }
      { Cost = 50; Damage = 2; Armor = 0 }
      { Cost = 100; Damage = 3; Armor = 0 }
      { Cost = 20; Damage = 0; Armor = 1 }
      { Cost = 40; Damage = 0; Armor = 2 }
      { Cost = 80; Damage = 0; Armor = 3 } ]

let boss =
    { Damage = 8
      Armor = 2
      HitPoints = 100 }

let allPossiblePlayers weapons armors rings cheapestSetup=
    let setupFn = if cheapestSetup then min else max
    let defaultValue = if cheapestSetup then System.Int32.MaxValue else 0
    let mutable playersCost = Map.empty
    let addSetup add player = 
        let value = setupFn add (if playersCost.ContainsKey player
                                 then playersCost.[player]
                                 else defaultValue)
        playersCost <- playersCost.Add(player, value)

    // 1 weapon; 0-1 armor; 0-2 rings
    let player = {Damage=0;Armor=0;HitPoints=100}
    for weapon in weapons do
        let pw = applyItem player weapon
        for armor in armors do
            let pwa = applyItem pw armor
            addSetup (weapon.Cost) pw
            addSetup (weapon.Cost + armor.Cost) pwa
            for ring1 in rings do
                let pwar = applyItem pwa ring1
                let pwr = applyItem pw ring1
                addSetup (weapon.Cost + ring1.Cost) pwr
                addSetup (weapon.Cost + armor.Cost + ring1.Cost) pwar
                for ring2 in rings do
                    if ring1<>ring2
                    then
                        let pwarr = applyItem pwar ring2
                        let pwrr = applyItem pwr ring2
                        addSetup (weapon.Cost + ring1.Cost + ring2.Cost) pwrr
                        addSetup (weapon.Cost + armor.Cost + ring1.Cost + ring2.Cost) pwarr

    playersCost
    
let fight c1 c2 =
    let damage (attacker:Character) (defender:Character) =
        max 1 (attacker.Damage - defender.Armor)
    let rec turn c1 c2 isC1Turn =
        if c1.HitPoints <= 0
        then "c2 wins"
        else
            if c2.HitPoints <= 0
            then "c1 wins"
            else
                match isC1Turn with
                | true ->
                    turn c1 {c2 with HitPoints=c2.HitPoints - (damage c1 c2)} false
                | false ->
                    turn {c1 with HitPoints=c1.HitPoints - (damage c2 c1)} c2 true
    "c1 wins" = turn c1 c2 true


// cheapest win
allPossiblePlayers weapons armors rings true
|> Map.toList
|> List.sortBy snd
|> List.filter (fun (c, _) -> fight c boss)
|> List.head

// most expensive lose
allPossiblePlayers weapons armors rings false
|> Map.toList
|> List.sortBy snd
|> List.filter (fun (c, _) -> not (fight c boss))
|> List.last


