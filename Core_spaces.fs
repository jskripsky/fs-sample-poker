module Poker.Core

/// == Card model ==
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
let sortedRanks = [           
  for v in 2..10 do yield (Value v)
  yield! [Jack; Queen; King; Ace]]


/// == Functions ==

// Deconstruction
let (rank, suit) = (fst, snd)

/// Check for flush (i.e. all cards share the same suit)
let isFlush hand =
  match hand with
  | (_, s)::tail -> tail |> Seq.forall (fun(_, x) -> x = s)
  | [] -> invalidArg "hand" "No cards."

/// Extract card ranks
let internal extractRanks: (Hand -> Rank list) = List.map rank

/// Sort ranks
let internal sortRanks = extractRanks >> List.sort

/// Find highest ranking card in hand
let internal highestRank = extractRanks >> List.max

/// Check for straight (five consecutively ascending ranks)
// TODO: handle case Ace = (Value 1)
let isStraight hand =
  let ranks = sortRanks hand
  let straight =
    sortedRanks
    |> Seq.skipWhile ((<>) ranks.[0])
    |> Seq.take 5
    |> Seq.toList
  ranks = straight

/// Group cards by rank, sort first by count, then rank
let internal groupByRank hand =
  hand
  |> sortRanks
  |> Seq.groupBy id
  |> Seq.map (fun (k, v) -> (Seq.length v, k))
  |> Seq.sortBy fst
  |> Seq.toList

/// Calculate the hand's category
let categorize hand =
  if List.length hand <> 5 then invalidArg "hand" "Hand does not hold five cards."
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

let compareHands h1 h2 =
  let (c1, c2) = (categorize h1, categorize h2)
  match compare c1 c2 with
  | 0 ->  // equal categories
    match c1 with
    | HighCard | OnePair | TwoPair | ThreeOfAKind
    | FullHouse | FourOfAKind -> compare (groupByRank h1) (groupByRank h2)
    | _ -> compare (highestRank h1) (highestRank h2)
  | c -> c  // different categories
