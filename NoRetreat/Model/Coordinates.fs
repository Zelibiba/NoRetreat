namespace NoRetreat


[<Struct>]
type Coordinates =
    { R: int
      C: int }

    member this.H = -this.R - this.C

    static member (+)({ R = row1; C = column1 }, { R = row2; C = column2 }) =
        { R = row1 + row2
          C = column1 + column2 }

    static member (-)({ R = row1; C = column1 }, { R = row2; C = column2 }) =
        { R = row1 - row2
          C = column1 - column2 }

module Coordinates =

    let create (row, column) = { R = row; C = column }

    let fromString (str: string) =
        str.Split(',')
        |> Array.map System.Int32.TryParse
        |> function
            | [| (true, r); (true, c) |] -> create (r, c)
            | _ -> failwithf "Can't parse to Coordinates: %s" str
        
            

    let private adjacentCoordsPattern =
        [ (1, -1); (0, -1); (1, 0); (-1, 1); (0, 1); (-1, 0) ]
        |> List.map create
        |> List.toSeq

    let adjacentCoords coords =
        Seq.map ((+) coords) adjacentCoordsPattern
