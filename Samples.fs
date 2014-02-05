/// Sample data
let someHand = [(v 9, ``♦``); (v 5, ``♦``); (A, ``♠``); (v 2, ``♦``); (J, ``♣``)]
let twoPairsHand = [(A, ``♠``); (J, ``♦``); (v 9, ``♦``); (v 9, ``♥``); (J, ``♣``)]
let threeOfAKindHand = [(v 4, ``♦``); (v 10, ``♥``); (v 4, ``♥``); (Q, ``♥``); (v 4, ``♠``)]
let straightHand = [(v 9, ``♦``) ; (v 10, ``♥``); (J, ``♥``); (Q, ``♥``); (K, ``♠``)]
let fullHouseHand = [(J, ``♦``); (v 9, ``♠``); (v 9, ``♥``); (J, ``♣``); (v 9, ``♦``)]

let flushHand = [v 10; J; v 4; Q; v 8]; |> List.map (fun r -> (r, ``♦``))

let fourOfAKindHand = [(J, ``♦``); (v 6, ``♠``); (v 6, ``♥``); (v 6, ``♣``); (v 6, ``♦``)]
let straightFlushHand = [v 7; v 8; v 9; v 10; J] |> List.map (fun r -> (r, ``♥``))
let royalFlushHand = [v 10; J; Q; K; A] |> List.map (fun r -> (r, ``♠``))

let ioPairs = [
	royalFlushHand, RoyalFlush
	straightFlushHand, StraightFlush
	fourOfAKindHand, FourOfAKind
	fullHouseHand, FullHouse
	flushHand, Flush
	straightHand, Straight
	threeOfAKindHand, ThreeOfAKind]

ioPairs |> List.forall (fun (hand, cat) -> categorize hand = cat)

/// Tests
isStraight someHand = false
isFlush flushHand = true


