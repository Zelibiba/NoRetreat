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
