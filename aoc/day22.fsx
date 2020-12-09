// apply, decrease, remove if 0
type Effect =
    | Poisened of int * int
    | Shielded of int * int
    | Recharging of int * int

type Character =
    { Name: string
      Damage: int
      HitPoints: int
      Effects: Effect list
      Mana: int }

let applyEffect effect c = { c with Effects = effect :: c.Effects }

let armor c =
    match c.Effects
          |> List.filter (fun x ->
              match x with
              | Shielded _ -> true
              | _ -> false)
          |> List.tryHead with
    | Some (Shielded (x, v)) when x > 0 -> v
    | _ -> 0

let handleEffect c effect =
    match effect with
    | Poisened (turns, damage) ->
        Poisened(turns - 1, damage),
        turns > 1,
        { c with
              HitPoints = c.HitPoints - damage }
    | Shielded (turns, armor) -> Shielded(turns - 1, armor), turns > 1, c
    | Recharging (turns, mana) -> Recharging(turns - 1, mana), turns > 1, { c with Mana = c.Mana + mana }

let handleEffects c =
    armor c,
    c.Effects
    |> List.fold (fun c' effect ->
        match handleEffect c' effect with
        | effect', true, c'' ->
            { c'' with
                  Effects = effect' :: c''.Effects }
        | _, false, c'' -> c'') { c with Effects = [] }

let cast mana c = { c with Mana = c.Mana - mana }

let heal amount c =
    { c with
          HitPoints = c.HitPoints + amount }

let takeDamage damage c =
    { c with
          HitPoints = c.HitPoints - damage }

let attack damage shield c = takeDamage (max 1 (damage - shield)) c

let isDead c = c.HitPoints <= 0

let poisened c =
    List.exists (fun x ->
        match x with
        | Poisened _ -> true
        | _ -> false) c.Effects

let shielded c =
    List.exists (fun x ->
        match x with
        | Shielded _ -> true
        | _ -> false) c.Effects

let recharging c =
    List.exists (fun x ->
        match x with
        | Recharging _ -> true
        | _ -> false) c.Effects

let fightToDeath wizard boss isHard=
    let mutable wins = Set.empty

    let rec turn manaSpent c1 c2 isC1Turn =
        let (shield, c1') = handleEffects (if isC1Turn && isHard then (takeDamage 1 c1) else c1)
        let (_, c2') = handleEffects c2

        if isDead c1' || c1'.Mana < 0 then ()
        else
            if isDead c2' then wins <- Set.add manaSpent wins
            else
                match isC1Turn with
                | true ->
                    if c1'.Mana < 53 then
                        ()
                    else
                        turn (manaSpent + 53) (c1' |> cast 53) (c2' |> takeDamage 4) false
                        turn (manaSpent + 73) (c1' |> cast 73 |> heal 2) (c2' |> takeDamage 4) false

                        if not (shielded c1')
                        then turn (manaSpent + 113) (c1' |> cast 113 |> applyEffect (Shielded(6, 7))) c2' false
                        if not (poisened c2')
                        then turn (manaSpent + 173) (c1' |> cast 173) (c2' |> applyEffect (Poisened(6, 3))) false
                        if not (recharging c1')
                        then turn (manaSpent + 229) (c1' |> cast 229 |> applyEffect (Recharging(5, 101))) c2' false
                | false -> turn manaSpent (c1' |> attack c2'.Damage shield) c2' true
    turn 0 wizard boss true
    wins

let wizard =
    { Name = "wizard"
      Damage = 0
      Mana = 500
      HitPoints = 50
      Effects = [] }

let boss =
    { Name = "boss"
      Damage = 10
      Mana = 1
      HitPoints = 71
      Effects = [] }

fightToDeath wizard boss false
|> Set.toList |> List.min

fightToDeath wizard boss true
|> Set.toList |> List.min