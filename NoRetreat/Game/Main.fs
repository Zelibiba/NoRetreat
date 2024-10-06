namespace NoRetreat.Game
open NoRetreat
open NoRetreat.Game

open Elmish
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

type T = 
    { Field: Field.T
      CardDeck: CardDeck.T

      Phase: Phase

      Player: Player.T
      Enemy: Player.T }

type Msg =
    | NextPhase
    | FieldMsg of Field.Msg
    | CardMsg of int * Card.Msg

module Main =

    let init () =
        let cards, cardDeck = List.mapFold (fun deck _ -> CardDeck.draw deck) (CardDeck.create ()) [1..4]

        { Field = Field.Main.init () |> fst
          CardDeck = cardDeck

          Phase = CardsPhase_Discard

          Player = Player.create Germany cards
          Enemy = Player.create USSR [] }, Cmd.none


    let update (msg: Msg) (state: T) =
        match msg with
        | NextPhase -> 
            let player' = Player.deselectCards state.Player
            { state with Player = player'; Phase = state.Phase.Next }, Cmd.none
        | FieldMsg fieldMsg ->
            let field, cmd = Field.Main.update fieldMsg state.Field
        
            { state with Field = field }, Cmd.map FieldMsg cmd
        | CardMsg (idx, Card.Play)
        | CardMsg (idx, Card.Discard) when state.Phase = CardsPhase_Discard ->
            let player', card = Player.deleteCard state.Player idx
            let deck' = CardDeck.discard state.CardDeck card
            let cmd =
                if player'.Cards.Length > 2 
                then Cmd.none
                else Cmd.batch [Cmd.ofMsg NextPhase; Cmd.ofMsg NextPhase]

            { state with Player = player'; CardDeck = deck' }, cmd
        | CardMsg (idx, cardMsg) ->
            let player' = Player.updateCard cardMsg state.Player idx
            { state with Player = player' }, Cmd.none


    let view (state: T) (dispatch: Msg -> unit) : IView =
        let dispatchCard = Library.dispatchwithIndex dispatch CardMsg

        Panel.create [
            Panel.children [
                Field.Main.view (state.Field, state.Phase) (FieldMsg >> dispatch)

                StackPanel.create [
                    StackPanel.height 200
                    StackPanel.horizontalAlignment HorizontalAlignment.Center
                    StackPanel.verticalAlignment VerticalAlignment.Bottom
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.children (
                        List.mapi (fun idx card -> Card.view card (dispatchCard idx)) state.Player.Cards
                    )
                ]
            ]
        ]