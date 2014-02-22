module Poker.Core

/// == Card model ==
[<NoComparison>]
type Suit =
	| Spade		/// ♠
	| Heart		/// ♥
	| Diamond	/// ♦
	| Club		/// ♣

type Rank =
	| Num of v: int  // v ∈ [2..10]
	| Jack | Queen | King | Ace

type Card = Rank * Suit
type Hand = Card list  // (h: Hand).Length = 5

/// Convenience aliases
let (``♠``, ``♥``, ``♦``, ``♣``) = (Spade, Heart, Diamond, Club)
let (n, J, Q, K, A) = (Num, Jack, Queen, King, Ace)

/// == Valuation model ==
type HandCategory =
	| HighCard		// no matching ranks, no straight, no flush
	| OnePair		// 2 + 1 + 1+ 1
	| TwoPair		// 2 + 2 + 1
	| ThreeOfAKind	// 3 + 1 + 1
	| Straight		// [r; r + 1; ... r + 4]
	| Flush			// matching suits
	| FullHouse		// 3 + 2
	| FourOfAKind	// 4 + 1
	| StraightFlush
	| RoyalFlush		// straight flush down from Ace


/// == Nums ==

/// rankOrder = [n 2; n 3; n 4; n 5; n 6; v7; n 8; n 9; n 10; J; Q; K; A]
/// rankOrder.Length = 13
let rankOrder =
	[for n in 2..10 do yield (Num n)
	 yield! [Jack; Queen; King; Ace]]

/// Staight starting with Ace as one (but sorted using regular ordering)
let internal straightFromOne = [n 2; n 3; n 4; n 5; Ace]

/// == Functions ==

/// Input validation

/// checkCard (v 13; Spade) => Exception
let inline checkCard card =
	match card with
	| (Num x, _) when (x < 2 || 10 < x) -> invalidArg "card" (sprintf "Invalid number card: %A" card)
	| _ -> ()

/// checkHand [(Ace, Spade)] => Exception
let inline checkHand hand =
	hand |> List.iter checkCard
	if List.length hand = 5 then () else invalidArg "hand" "Hand does not conain five cards."

/// Get card ranks
/// getRanks [(A, ``♠``); (J, ``♠``); (n 2, ``♠``); (n 5, ``♠``); (A, ``♣``)] = [A; J; n 2; n 5; A]
let internal getRanks (cards: Card list) = cards |> List.map fst

/// Sort ranks
/// sortRanks [(A, ``♠``); (J, ``♠``); (n 2, ``♠``); (n 5, ``♠``); (A, ``♣``)] = [n 2; n 5; J; A; A]
let internal sortRanks = getRanks >> List.sort

/// Find highest ranking card in hand
/// highestRank [(A, ``♠``); (J, ``♠``); (n 2, ``♠``); (n 5, ``♠``); (A, ``♣``)] = A
let internal highestRank hand =
	let ranks = hand |> sortRanks
	if ranks <> straightFromOne then
		ranks |> List.max
	else
		(n 5)  // straight with Ace = 1

/// Check for flush (i.e. all cards share the same suit)
/// isFlush [(A, ``♠``); (J, ``♠``); (n 2, ``♠``); (n 5, ``♠``); (n 4, ``♣``)] = false
/// isFlush [(A, ``♠``); (J, ``♠``); (n 2, ``♠``); (n 5, ``♠``); (n 4, ``♠``)] = true
let isFlush hand =						/// hand = [(A, ``♠``); (J, ``♠``); (n 2, ``♠``); (n 5, ``♠``); (n 4, ``♣``)]
	checkHand hand
	let suits = hand |> List.map snd		/// suits = [``♠``; ``♠``; ``♠``; ``♠``; ``♣``]
	// Note: we could also use (suits |> Set.ofList |> Set.count) = 1 instead of (::) and Seq.forall
	match suits with
	| s::tail -> tail |> List.forall ((=) s)		/// [``♠``; ``♠``; ``♠``; ``♣``] |> Seq.forall ((=) ``♠``) = false
	| [] -> invalidArg "hand" "No cards."


/// Check for straight (five consecutively ascending ranks)
/// isStraight [(A, ``♠``); (J, ``♠``); (n 2, ``♠``); (n 5, ``♠``); (A, ``♣``)] = false
/// isStraight [(n 8, ``♠``); (Q, ``♦``); (n 9, ``♦``); (n 10, ``♥``); (J, ``♣``)] = true

let isStraight hand =				/// hand = [(n 8, ``♠``); (Q, ``♦``); (n 9, ``♦``); (n 10, ``♥``); (J, ``♣``)]
	checkHand hand
	let ranks = sortRanks hand		/// ranks = [n 8; n 9; n 10; J; Q]
	let generateStraight start =		/// start = n 8
		rankOrder				///= [n 2; n 3; n 4; n 5; n 6; n 7; n 8; n 9; n 10; J; Q; K; A]
		|> Seq.skipWhile ((<>) start)	///= seq [n 8; n 9; n 10; J; Q; K; A]
		|> Seq.take 5				///= seq [n 8; n 9; n 10; J; Q]
		|> Seq.toList				///= [n 8; n 9; n 10; J; Q]
	ranks = generateStraight (ranks.[0])		///= true
	|| ranks = straightFromOne  // special case Ace = v 1

/// Group cards by rank, sort first by count, then rank
let internal groupByRank hand =
	let swap (x: Rank, y: int) = (y, x)
	hand			///= [(A, ``♠``); (J, ``♦``); (n 9, ``♦``); (n 9, ``♥``); (J, ``♣``)]
	|> sortRanks		///= [n 9; n 9; J; J; A]
	|> Seq.countBy id 	///= seq [(n 9, 2); (J, 2); (A, 1)]
	|> Seq.toList		///= [(n 9, 2); (J, 2); (A, 1)]
	|> List.map swap	///= [(2, n 9); (2, J); (1, A)]
	|> List.sortBy fst	///= [(1, A); (2, n 9); (2, J)]

/// [(A, ``♠``); (J, ``♦``); (n 9, ``♦``); (n 9, ``♥``); (J, ``♣``)] => [(1, A); (2, n 9); (2, J)]
/// [(n 4, ``♦``); (n 10, ``♥``); (n 4, ``♥``); (Q, ``♥``); (n 4, ``♠``)] => [(1, n 10); (1, Q); (3, n 4)]

/// Calculate the hand's category
let categorizeHand hand =	 /// hand = [(A, ``♠``); (J, ``♦``); (n 9, ``♦``); (n 9, ``♥``); (J, ``♣``)]
	match (isStraight hand, isFlush hand) with		/// (false, false)
	| (false, false) ->							///  /* this branch taken */
		let groupLengths =
			hand							/// [(A, ``♠``); (J, ``♦``); (n 9, ``♦``); (n 9, ``♥``); (J, ``♣``)]
			|> groupByRank					/// [(1, A); (2, n 9); (2, J)]
			|> List.map fst					/// [1; 2; 2]
			|> List.rev						/// [2; 2; 1]  // (inverted ordering)
		match groupLengths with				/// note: 'groupLengths' are sorted in _descending_ order
		| 1::_ -> HighCard
		| 2::1::_ -> OnePair
		| 2::2::_ -> TwoPair					/// TwoPair
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
		| _ -> compare (highestRank h1) (highestRank h2)  // Note: this handles staights with Ace = (Num 1) as well.
	| c -> c  // different categories
