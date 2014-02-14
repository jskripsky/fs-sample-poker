/// Sample data
let someHand = [(n 9, ``♦``); (n 5, ``♦``); (A, ``♠``); (n 2, ``♦``); (J, ``♣``)]
let twoPairsHand = [(A, ``♠``); (J, ``♦``); (n 9, ``♦``); (n 9, ``♥``); (J, ``♣``)]
let threeOfAKindHand = [(n 4, ``♦``); (n 10, ``♥``); (n 4, ``♥``); (Q, ``♥``); (n 4, ``♠``)]
let straightHand = [(n 9, ``♦``) ; (n 10, ``♥``); (J, ``♥``); (Q, ``♥``); (K, ``♠``)]
let fullHouseHand = [(J, ``♦``); (n 9, ``♠``); (n 9, ``♥``); (J, ``♣``); (n 9, ``♦``)]

let flushHand = [n 10; J; n 4; Q; n 8]; |> List.map (fun r -> (r, ``♦``))

let fourOfAKindHand = [(J, ``♦``); (n 6, ``♠``); (n 6, ``♥``); (n 6, ``♣``); (n 6, ``♦``)]
let straightFlushHand = [n 7; n 8; n 9; n 10; J] |> List.map (fun r -> (r, ``♥``))
let royalFlushHand = [n 10; J; Q; K; A] |> List.map (fun r -> (r, ``♠``))

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


