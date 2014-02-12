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

isFlush -> checkHand, (|>),  List.map, snd, List<'T>.(::), Seq.forall, (=), invalidArg  // 7 = (1) + 6
isStraight -> checkHand, sortRanks, rankOrder, (|>), Seq.skipWhile, (<>), Seq.take, Seq.toList, straightFromOne  // 9 = (4) + 5

groupByRank (internal) -> sortRanks, (|>), Seq.countBy, id, Seq.map, Seq.sortBy, fst, Seq.toList  // 8 = (1) + 7
categorize -> isStraight, isFlush, (|>), groupByRank, List.rev, List<'T>.(::), invalidArg, highestRank  // 8 = (4) + 4
compareHands -> categorize, compare, groupByRank, highestRank  // 4 = (3) + 1

internal = straightFromOne, checkHand, extractRank, sortRanks, highestRank, groupdByRank
public = rankOrder, isFlush, isStraight, categorize, compareHands

Microsoft.FSharp =
	Core.Operators,
	Collections

Collections = List, Seq, List<'T>.(::)

Core.Operators = /// 9
	fst, snd,
	compare, id,
	(=), (<>),
	(|>), (>>),
	invalidArg

List = length, map, sort, rev /// 4
Seq = forall, skipWhile, take, toList, countBy, map, sortBy /// 7
