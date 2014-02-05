namespace Poker
  module Core = begin
    type Suit =
      | Spade
      | Heart
      | Diamond
      | Club
    type Rank =
      | Value of int
      | Jack
      | Queen
      | King
      | Ace
    val ♦ : Suit
    val ♥ : Suit
    val ♣ : Suit
    val ♠ : Suit
    val v : (int -> Rank)
    val Q : Rank
    val K : Rank
    val J : Rank
    val A : Rank
    type Card = Rank * Suit
    type Hand = Card list
    type HandCategory =
      | HighCard
      | OnePair
      | TwoPair
      | ThreeOfAKind
      | Straight
      | Flush
      | FullHouse
      | FourOfAKind
      | StraightFlush
      | RoyalFlush
    val sortedRanks : Rank list
    val suit : ('c * 'd -> 'd)
    val rank : ('a * 'b -> 'a)
    val isFlush : hand:('a * 'b) list -> bool when 'b : equality
    val internal extractRanks : (Hand -> Rank list)
    val internal sortRanks : (Hand -> Rank list)
    val internal highestRank : (Hand -> Rank)
    val isStraight : hand:Hand -> bool
    val internal groupByRank : hand:Hand -> (int * Rank) list
    val categorize : hand:Card list -> HandCategory
    val compareHands : h1:Card list -> h2:Card list -> int
  end

