module Game

/// Game model
type Deck = Card list
type Player = {Name: string; Hand: Hand}
type Game = {Deck: Deck; Players: Player list}

type ShuffleDeck = Deck -> Deck
type Deal = Deck -> (Deck * Card)
type Pickup = (Hand * Card) -> Hand

let sortedFullDeck' =
	[for s in [Spade; Heart; Diamond; Club] do 
		for r in rankOrder do
			yield (r, s)]

/// Shuffle randomly
let shuffle cards =
	let rand = new System.Random()
	cards
	|> List.map (fun c -> (rand.Next(), c))
	|> List.sortBy fst
	|> List.map snd

