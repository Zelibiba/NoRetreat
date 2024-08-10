namespace NoRetreat

[<Struct>]
type Country =
    | USSR
    | Germany

    static member fromString(str) =
        match str with
        | "USSR" -> USSR
        | "Germany" -> Germany
        | _ -> failwithf "Can't parse to Country: %s" str