module NoRetreat.Game.Player
open NoRetreat
open NoRetreat.Game

type T = 
    { Country: Country
      Cards: Card.T list }

let create country cards =
    { Country = country
      Cards = cards }

let addCards (player: T) cards =
    { player with Cards = player.Cards @ cards }

let deleteCard (player: T) index =
    let card = player.Cards[index]
    let cards' = List.removeAt index player.Cards
    { player with Cards = cards' }, card

let updateCard msg player index =
    let card' = Card.update msg player.Cards[index]
    let cards' = List.updateAt index card' player.Cards
    { player with Cards = cards' }

let deselectCards (player: T) =
    player.Cards
    |> List.toSeq
    |> Seq.indexed
    |> Seq.filter (snd >> _.Selection >> (=) CanBeSelected)
    |> Seq.map fst
    |> Seq.fold (updateCard <| Card.SetSelection NotSelected) player