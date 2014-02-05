module Poker.Core

/// == Card model ==
[<NoComparison>]
type Suit =
	| Spade  // ♠
	| Heart  // ♥
	| Diamond  // ♦
	| Club  // ♣

type Rank =
	| Value of int (* 2..10 *)
	| Jack | Queen | King | Ace

let (``♠``, ``♥``, ``♦``, ``♣``) = (Spade, Heart, Diamond, Club)
let (v, J, Q, K, A) = (Value, Jack, Queen, King, Ace)

type Card = Rank * Suit
type Hand = Card list

/// == Valuation model ==
type HandCategory =
	| HighCard // no matching ranks, no straight or flush
	| OnePair  // 2 + 1 + 1+ 1
	| TwoPair  // 2 + 2 + 1
	| ThreeOfAKind  // 3 + 1 + 1
	| Straight  // r..(r + 4)
	| Flush  // matching suits
	| FullHouse // 3 + 2
	| FourOfAKind  // 4 + 1
	| StraightFlush
	| RoyalFlush  // straight flush down from Ace


/// == Values ==

/// seq [v 2; v 3; v 4; v 5; v 6; v7; v 8; v 9; v 10; J; Q; K; A]
let rankOrder = [           
	for v in 2..10 do yield (Value v)
	yield! [Jack; Queen; King; Ace]]


/// == Functions ==

/// Check for flush (i.e. all cards share the same suit)
let isFlush hand =						/// hand = [(A, ``♠``); (J, ``♠``); (v 2, ``♠``); (v 5, ``♠``); (A, ``♣``)]
	let suits = hand |> List.map snd		/// suits = [``♠``; ``♠``; ``♠``; ``♠``; ``♣``]
	match suits with
	| s::tail -> tail |> Seq.forall ((=) s)		/// [``♠``; ``♠``; ``♠``; ``♣``] |> Seq.forall ((=) ``♠``) = false
	| [] -> invalidArg "hand" "No cards."

/// [(A, ``♠``); (J, ``♠``); (v 2, ``♠``); (v 5, ``♠``); (A, ``♣``)] => false

/// Extract card ranks
let internal extractRanks: (Hand -> Rank list) = List.map fst

/// Sort ranks
let internal sortRanks = extractRanks >> List.sort

/// Find highest ranking card in hand
let internal highestRank = extractRanks >> List.max

/// Check for straight (five consecutively ascending ranks)
// TODO: handle case Ace = (Value 1)
let isStraight hand =				/// hand = [(v 8, ``♠``); (Q, ``♦``); (v 9, ``♦``); (v 10, ``♥``); (J, ``♣``)]
	let ranks = sortRanks hand		/// ranks = [v 8; v 9; v 10; J; Q]
	let straightStartingWith r =		/// r = v 8
		rankOrder				///= seq [v 2; v 3; v 4; v 5; v 6; v7; v 8; v 9; v 10; J; Q; K; A]
		|> Seq.skipWhile ((<>) r)	///= seq [v 8; v 9; v 10; J; Q; K; A]
		|> Seq.take 5				///= seq [v 8; v 9; v 10; J; Q]
		|> Seq.toList				///= [v 8; v 9; v 10; J; Q]
	ranks = straightStartingWith (ranks.[0])		///= true

/// Group cards by rank, sort first by count, then rank
let internal groupByRank hand =
	let swap (x: Rank, y: int) = (y, x)
	hand			///= [(A, ``♠``); (J, ``♦``); (v 9, ``♦``); (v 9, ``♥``); (J, ``♣``)]
	|> sortRanks		///= [v 9; v 9; J; J; A]
	|> Seq.countBy id	///= seq [(v 9, 2); (J, 2); (A, 1)]
	|> Seq.map swap	///= seq [(2, v 9); (2, J); (1, A)]
	|> Seq.sortBy fst	///= seq [(1, A); (2, v 9); (2, J)]
	|> Seq.toList		///= [(1, A); (2, v 9); (2, J)]

/// [(A, ``♠``); (J, ``♦``); (v 9, ``♦``); (v 9, ``♥``); (J, ``♣``)] => [(1, A); (2, v 9); (2, J)]
/// [(v 4, ``♦``); (v 10, ``♥``); (v 4, ``♥``); (Q, ``♥``); (v 4, ``♠``)] => [(1, v 10); (1, Q); (3, v 4)]

/// Calculate the hand's category
let categorize hand =
	if List.length hand <> 5 then invalidArg "hand" "Hand does not conain five cards."
	match (isStraight hand, isFlush hand) with
	| (false, false) -> 
		match hand |> groupByRank |> List.rev with
		| (1, _)::_ -> HighCard
		| (2, _)::(1, _)::_ -> OnePair
		| (2, _)::(2, _)::_ -> TwoPair
		| (3, _)::(1, _)::_ -> ThreeOfAKind
		| (3, _)::(2, _)::_ -> FullHouse
		| (4, _)::_ -> FourOfAKind
		| _ -> invalidArg "hand" "Hand contains more then five cards."
	| (true, false) -> Straight
	| (false, true) -> Flush
	| (true, true) -> if highestRank hand <> Ace then StraightFlush else RoyalFlush

// TODO: handle case Ace = (Value 1)
let compareHands h1 h2 =
	let (c1, c2) = (categorize h1, categorize h2)
	match compare c1 c2 with
	| 0 ->  // equal categories
		match c1 with
		| HighCard | OnePair | TwoPair | ThreeOfAKind
		| FullHouse | FourOfAKind -> compare (groupByRank h1) (groupByRank h2)
		| _ -> compare (highestRank h1) (highestRank h2)
	| c -> c  // different categories