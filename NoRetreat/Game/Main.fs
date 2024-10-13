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
      CanSwitchToNextPhase: bool

      Player: Player.T
      Enemy: Player.T }

type Msg =
    | NextPhase
    | FieldMsg of Field.Msg
    | CardMsg of int * Card.Msg

module Main =

    let toNextPhase (game: T) =
        let player' = Player.deselectCards game.Player
        let phase' = game.Phase.Next
        { game with 
            Player = player'
            Phase = phase'
            CanSwitchToNextPhase = Phase.canSwitchToNext phase'}

    let init () =
        let cards, cardDeck = CardDeck.create () |> CardDeck.drawMany 4

        { Field = Field.Main.init () |> fst
          CardDeck = cardDeck

          Phase = CardsPhase true
          CanSwitchToNextPhase = false

          Player = Player.create Germany cards
          Enemy = Player.create USSR [] }, Cmd.none


    let update (msg: Msg) (state: T) =
        match msg with
        | NextPhase -> 
            let state' = toNextPhase state
            state', Cmd.none
        | FieldMsg fieldMsg ->
            let field, cmd = Field.Main.update fieldMsg state.Field
        
            { state with Field = field; }, Cmd.map FieldMsg cmd
        | CardMsg (idx, Card.Play)
        | CardMsg (idx, Card.Discard) when state.Phase = CardsPhase true ->
            let player, card = Player.deleteCard state.Player idx
            let deck = CardDeck.discard card state.CardDeck
            let player', deck', definePhase =
                if player.Cards.Length > 2 then
                    player, deck, id
                else
                    let cards, deck' = CardDeck.drawMany 4 deck
                    let player' = Player.addCards player cards
                    player', deck', toNextPhase

            definePhase { state with Player = player'; CardDeck = deck' }, Cmd.none
        | CardMsg (idx, cardMsg) ->
            let player' = Player.updateCard cardMsg state.Player idx
            { state with Player = player' }, Cmd.none


    let view (state: T) (dispatch: Msg -> unit) : IView =
        let dispatchCard = Library.dispatchwithIndex dispatch CardMsg

        Panel.create [
            Panel.children [
                DockPanel.create [
                    DockPanel.children [
                        DockPanel.create [
                            DockPanel.dock Dock.Top
                            DockPanel.lastChildFill false
                            DockPanel.height 30
                            DockPanel.children [
                                Border.create [
                                    Border.dock Dock.Left
                                    Border.width 200
                                    Border.borderBrush "black"
                                    Border.borderThickness 1
                                    Border.child (TextBlock.create [
                                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                                        TextBlock.verticalAlignment VerticalAlignment.Center
                                        TextBlock.text <| sprintf "%A" state.Phase
                                    ])
                                ]
                                Border.create [
                                    Border.dock Dock.Left
                                    Border.height 30
                                    Border.borderBrush "black"
                                    Border.borderThickness 1
                                    Border.child (Button.create [
                                        Button.content "|>"
                                        Button.isEnabled state.CanSwitchToNextPhase
                                        Button.onClick (fun _ -> dispatch NextPhase)
                                    ])
                                ]
                                Border.create [
                                    Border.dock Dock.Right
                                    Border.padding (3, 0, 0, 3)
                                    Border.borderBrush "black"
                                    Border.borderThickness 1
                                    Border.child (TextBlock.create [
                                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                                        TextBlock.verticalAlignment VerticalAlignment.Center
                                        TextBlock.text <| sprintf "DrawPile: %i, DiscardPile: %i" state.CardDeck.DrawPile.Length state.CardDeck.DiscardPile.Length
                                    ])
                                ]
                            ]
                        ]
                        Field.Main.view state.Field (FieldMsg >> dispatch)
                    ]
                ]

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