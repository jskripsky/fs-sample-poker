module Poker.Core

/// == Card model ==
[<NoComparison>]
type Suit =
	| Spade		// ♠
	| Heart		// ♥
	| Diamond	// ♦
	| Club		// ♣

type Rank =
	| Value of v: int  // assert v ∈ [2..10]
	| Jack | Queen | King | Ace

let (``♠``, ``♥``, ``♦``, ``♣``) = (Spade, Heart, Diamond, Club)
let (v, J, Q, K, A) = (Value, Jack, Queen, King, Ace)

type Card = Rank * Suit
type Hand = Card list  // assert (h: Hand).Length = 5

/// == Valuation model ==
type HandCategory =
	| HighCard		// no matching ranks, no straight or flush
	| OnePair		// 2 + 1 + 1+ 1
	| TwoPair		// 2 + 2 + 1
	| ThreeOfAKind	// 3 + 1 + 1
	| Straight		// r..(r + 4)
	| Flush			// matching suits
	| FullHouse		// 3 + 2
	| FourOfAKind	// 4 + 1
	| StraightFlush
	| RoyalFlush		// straight flush down from Ace


/// == Values ==

/// [v 2; v 3; v 4; v 5; v 6; v7; v 8; v 9; v 10; J; Q; K; A]
let rankOrder = [
	for v in 2..10 do
		yield (Value v)
		yield! [Jack; Queen; King; Ace]]

/// Staight starting with Ace as one (but sorted using regular ordering)
let internal straightFromOne = [v 2; v 3; v 4; v 5; Ace]

/// == Functions ==

/// Input validation
let inline checkCard card =
	match card with
	| (Value x, _) -> if x <= 2 && 10 <= x then () else invalidArg "hand" "Hand does not conain five cards."
	| _ -> ()

let inline checkHand hand =
	hand |> List.iter checkCard
	if List.length hand = 5 then () else invalidArg "hand" "Hand does not conain five cards."

/// Extract card ranks
let internal getRanks (cards: Card list) = cards |> List.map fst

/// Sort ranks
let internal sortRanks = getRanks >> List.sort

/// Find highest ranking card in hand
let internal highestRank hand =
	let ranks = hand |> sortRanks
	if ranks <> straightFromOne then
		ranks |> List.max
	else
		(v 5)  // straight with Ace = 1

/// Check for flush (i.e. all cards share the same suit)
let isFlush hand =						/// hand = [(A, ``♠``); (J, ``♠``); (v 2, ``♠``); (v 5, ``♠``); (A, ``♣``)]
	checkHand hand
	let suits = hand |> List.map snd		/// suits = [``♠``; ``♠``; ``♠``; ``♠``; ``♣``]
	// Note: we could also use (suits |> Set.ofList |> Set.count) = 1 instead of (::) and Seq.forall
	match suits with
	| s::tail -> tail |> List.forall ((=) s)		/// [``♠``; ``♠``; ``♠``; ``♣``] |> Seq.forall ((=) ``♠``) = false
	| [] -> invalidArg "hand" "No cards."

/// [(A, ``♠``); (J, ``♠``); (v 2, ``♠``); (v 5, ``♠``); (A, ``♣``)] => false

/// Check for straight (five consecutively ascending ranks)
let isStraight hand =				/// hand = [(v 8, ``♠``); (Q, ``♦``); (v 9, ``♦``); (v 10, ``♥``); (J, ``♣``)]
	checkHand hand
	let ranks = sortRanks hand		/// ranks = [v 8; v 9; v 10; J; Q]
	let generateStraight start =		/// start = v 8
		rankOrder				///= [v 2; v 3; v 4; v 5; v 6; v 7; v 8; v 9; v 10; J; Q; K; A]
		|> Seq.skipWhile ((<>) start)	///= seq [v 8; v 9; v 10; J; Q; K; A]
		|> Seq.take 5				///= seq [v 8; v 9; v 10; J; Q]
		|> Seq.toList				///= [v 8; v 9; v 10; J; Q]
	ranks = generateStraight (ranks.[0])		///= true
	|| ranks = straightFromOne  // special case Ace = v 1

/// Group cards by rank, sort first by count, then rank
let internal groupByRank hand =
	let swap (x: Rank, y: int) = (y, x)
	hand			///= [(A, ``♠``); (J, ``♦``); (v 9, ``♦``); (v 9, ``♥``); (J, ``♣``)]
	|> sortRanks		///= [v 9; v 9; J; J; A]
	|> Seq.countBy id 	///= seq [(v 9, 2); (J, 2); (A, 1)]
	|> Seq.map swap	///= seq [(2, v 9); (2, J); (1, A)]
	|> Seq.sortBy fst	///= seq [(1, A); (2, v 9); (2, J)]
	|> Seq.toList		///= [(1, A); (2, v 9); (2, J)]

/// [(A, ``♠``); (J, ``♦``); (v 9, ``♦``); (v 9, ``♥``); (J, ``♣``)] => [(1, A); (2, v 9); (2, J)]
/// [(v 4, ``♦``); (v 10, ``♥``); (v 4, ``♥``); (Q, ``♥``); (v 4, ``♠``)] => [(1, v 10); (1, Q); (3, v 4)]

/// Calculate the hand's category
let categorizeHand hand =	 /// hand = [(A, ``♠``); (J, ``♦``); (v 9, ``♦``); (v 9, ``♥``); (J, ``♣``)]
	match (isStraight hand, isFlush hand) with		/// (false, false)
	| (false, false) ->							/// /* this branch taken */
		let groupLengths =
			hand							/// [(A, ``♠``); (J, ``♦``); (v 9, ``♦``); (v 9, ``♥``); (J, ``♣``)]
			|> groupByRank					/// [(1, A); (2, v 9); (2, J)]
			|> List.map fst					/// [1; 2; 2]
			|> List.rev						/// [2; 2; 1]
		match groupLengths with
		| 1::_ -> HighCard
		| 2::1::_ -> OnePair
		| 2::2::_ -> TwoPair						/// TwoPair
		| 3::1::_ -> ThreeOfAKind
		| 3::2::_ -> FullHouse
		| 4::_ -> FourOfAKind
		| _ -> invalidArg "hand" "Hand contains more then five cards."
	| (true, false) -> Straight
	| (false, true) -> Flush
	| (true, true) -> if highestRank hand <> Ace then StraightFlush else RoyalFlush

let compareHands h1 h2 =
	let (c1, c2) = (categorizeHand h1, categorizeHand h2)
	match compare c1 c2 with
	| 0 ->  // equal categories
		match c1 with
		| HighCard | OnePair | TwoPair | ThreeOfAKind
		| FullHouse | FourOfAKind -> compare (groupByRank h1) (groupByRank h2)
		| _ -> compare (highestRank h1) (highestRank h2)  // Note: this handles staights with Ace = (Value 1) as well.
	| c -> c  // different categories
