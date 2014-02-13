Card -> Rank, Suit
Hand -> Card, list

// ref dependency:
// # refs = (# local refs) + # global refs



rankOrder
straightFromOne (internal)

checkHand (internal) -> List.length, invalidArg  // 2
extractRanks (internal) -> List.map, fst  // 2

sortRanks (internal) -> extractRanks, (>>), List.sort  // 3 = (1) + 2
highestRank (internal) -> (|>), sortRanks, (<>), straightFromOne, List.max  // 5 = (2) + 3

isFlush -> checkHand, (|>),  List.map, snd, List<'T>.(::), List.forall, (=), invalidArg  // 7 = (1) + 6
isStraight -> checkHand, sortRanks, rankOrder, (|>), Seq.skipWhile, (<>), Seq.take, Seq.toList, straightFromOne  // 9 = (4) + 5

groupByRank (internal) -> sortRanks, (|>), Seq.countBy, id, Seq.toList, List.map, fst, List.sortBy   // 8 = (1) + 7
categorizeHand -> isStraight, isFlush, (|>), groupByRank, List.rev, List<'T>.(::), invalidArg, highestRank  // 8 = (4) + 4
compareHands -> categorizeHand, compare, groupByRank, highestRank  // 4 = (3) + 1

internal = straightFromOne, checkHand, extractRank, sortRanks, highestRank, groupdByRank
public = rankOrder, isFlush, isStraight, categorize, compareHands

Microsoft.FSharp =
	Core.Operators,
	Collections

Core.Operators = /// 9
	fst, snd,
	compare, id,
	(=), (<>),
	(|>), (>>),
	invalidArg

Collections = List, Seq  (* List<'T>.(::) merged into List *)

List = length, map, forAll, sort, sortBy, rev, (::) /// 7
Seq = skipWhile, take, toList, countBy /// 4

