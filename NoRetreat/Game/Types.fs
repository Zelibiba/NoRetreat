namespace NoRetreat.Game

[<Struct>]
type Country =
    | USSR
    | Germany

    member x.Opposite =
        match x with
        | USSR -> Germany
        | Germany -> USSR

    static member fromString(str) =
        match str with
        | "USSR" -> USSR
        | "Germany" -> Germany
        | _ -> failwithf "Can't parse to Country: %s" str

[<Struct>]
type Selection =
    | NotSelected
    | CanBeSelected
    | Selected

[<Struct>]
type Phase =
    | CardsPhase of discard: bool
    | SupplyPhase

    member x.Next =
        match x with
        | CardsPhase true -> CardsPhase false
        | CardsPhase false -> SupplyPhase
        | SupplyPhase -> x

    static member canSwitchToNext = function
        | CardsPhase discard -> not discard
        | SupplyPhase -> true
