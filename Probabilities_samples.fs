let histoCategoryForRandomHand =
	// number of ranks, suits, cards in hand, cards in full deck
	let (r, s, h, fd) = (13, 4, 5, 52)

	let rec fact = function
		| 0 -> 1
		| n -> n * fact (n - 1)

	let rec pick m = function
		| 0 -> 1
		| n -> m * pick (m - 1) (n - 1)

	let slice full sub = (pick full sub) / (fact sub)

	let allHands = slice fd h
	let rec exp x y =
		match y with
		| 0 -> 1
		| _ -> x * exp x (y - 1)

	[
		RoyalFlush, s
		StraightFlush, s * (r - h + 1)
		FourOfAKind, r * (fd - 4)
		FullHouse, (slice r 1) * (slice s 3) * (slice (r - 1) 1) * (slice s 2)
		Flush, s * (slice r h) - 40 (* straigh flushes *)
		Straight, (r - h + 2) * exp 4 5 - 40 (* straigh flushes *)  /// Note: Ace may be on start or end!
		ThreeOfAKind, (slice r 1) * (slice s 3) * (slice (r - 1) 2) * exp (slice s 1) 2
		TwoPair, (slice r 2) * (slice s 2) * (slice s 2) * (slice (r - 2) 1) * (slice s 1)
		OnePair, (slice r 1) * (slice s 2) * (slice (r - 1) 3) * exp s 3]  // pick a rank, take two card of that rank,...

let allHands = slice 52 5

let probCategoryForRandomHand =
	let all = float allHands
	histoCategoryForRandomHand
	|> List.map (fun (c, h) -> (c, float h / all))
