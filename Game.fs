module Game

/// Game model
type Deck = Card list
type Player = {Name: string; Hand: Hand}
type Game = {Deck: Deck; Players: Player list}

type ShuffleDeck = Deck -> Deck
type Deal = Deck -> (Deck * Card)
type Pickup = (Hand * Card) -> Hand

let sortedFullDeck =
	[for s in [Spade; Heart; Diamond; Club] do 
		for r in rankOrder do
			yield (r, s)]

/// Generate a random number between 0 (incl.) and Int32.MaxValue (excl).
let random () =
	let rand = new System.Random()
	rand.Next()

/// Shuffle randomly
let shuffle cards =
	cards
	|> List.map (fun c -> (random (), c))
	|> List.sortBy fst
	|> List.map snd

