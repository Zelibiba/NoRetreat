namespace NoRetreat.Game
open NoRetreat
open NoRetreat.Controls

open Avalonia.Controls
open Avalonia.Media.Imaging
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

module Card =
    [<Struct>]
    type ID = ID of int

    let createID id =
        if id > 0 && id <= 54 then
            ID id
        else
            failwithf "Can't create card with ID: %i" id

    [<Struct>]
    type T = 
        { ID: ID 
          Selection: Selection }

    let init id = 
        { ID = id 
          Selection = CanBeSelected }

    type Msg = 
        | SetSelection of Selection
        | Play
        | Discard

    let update (msg: Msg) (state: T) =
        match msg with
        | SetSelection selection -> { state with Selection = selection }
        | Play
        | Discard -> state

    module private Images =
        let card = Library.memoize <| fun (ID id) ->
            sprintf "avares://NoRetreat/Assets/Images/Cards/Card%02i.PNG" id
            |> Bitmap.create

    let view (state: T) dispatch : IView =
        Border.create [
            Border.margin 5
            Border.cornerRadius 10
            Border.renderTransformOrigin <| Avalonia.RelativePoint(0.5, 1, Avalonia.RelativeUnit.Relative)
            Border.classes ["Card"]
            Border.clipToBounds true
            Border.borderThickness 3
            Border.borderBrush (
                match state.Selection with
                | NotSelected -> "transparent"
                | CanBeSelected -> "green"
                | Selected -> "red"
            )
            Border.child (
                Image.create [
                    Image.source (Images.card state.ID)
                    if state.Selection = CanBeSelected then
                        Image.onPointerPressedExt2 (
                            EventLib.splitByLeftButton,
                            (fun e -> e.Handled <- true; dispatch Play),
                            (fun e -> e.Handled <- true; dispatch Discard)
                        )
                ]
            )
        ]

module CardDeck =

    type T =
        { DrawPile: Card.ID list
          DiscardPile: Card.ID list }

    let private shuffle (cards: _ list) =
        let length = cards.Length
        let indexes = Array.init length id
        let rnd = System.Random()
        for i in [0..(length-2)] do
            let j = rnd.Next(i, length-1)
            let tmp = indexes[i]
            indexes[i] <- indexes[j]
            indexes[j] <- tmp
        List.permute (Array.get indexes) cards

    let create () =
        { DrawPile = List.map Card.createID [1..44] |> shuffle
          DiscardPile = [] }

    let draw (cardDeck: T) =
        let cardDeck = 
            if cardDeck.DrawPile.Length > 1 then
                cardDeck
            else
                let cards = cardDeck.DrawPile @ cardDeck.DiscardPile |> shuffle
                { DrawPile = cards; DiscardPile = [] }

        Card.init cardDeck.DrawPile[0],
        { cardDeck with DrawPile = cardDeck.DrawPile.Tail }

    let drawMany count (cardDeck: T) =
        List.mapFold (fun deck _ -> draw deck) cardDeck [1..count]

    let discard (card: Card.T) (cardDeck: T) =
        { cardDeck with DiscardPile = card.ID :: cardDeck.DiscardPile }