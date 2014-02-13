module Poker.Probabilities

let filterInAnyOrder items dist =
	items
	|> Seq.fold (fun d item -> filter (Seq.exists ((=) (item))) d) dist
 
let map f dist = dist |> Seq.map (fun o -> { Value = f o.Value; Probability = o.Probability })
 
let selectOne values =
	[for e in values -> e, values |> Seq.filter ((<>) e)]
	|> toUniformDistribution
 
let rec selectMany n values =
	match n with
	| 0 -> certainly ([], values)
	| _ ->
		distribution {
			let! (x,c1) = selectOne values
			let! (xs,c2) = selectMany (n-1) c1
			return x::xs,c2 }

let select n values = selectMany n values |> map (fst >> List.rev)

let remove items = Seq.filter (fun v -> Seq.forall ((<>) v) items)


/// Tests
sortedFullDeck
|> select 2
|> filter ((=) [A,Club; A,Spade])
|> probability
|> printfn "%A" // prints "1/2652"
 
// draw Ace of Clubs and Ace of Spaces in any order
sortedFullDeck
|> select 2
|> filterInAnyOrder [A,Club; A,Spade]
|> probability
|> printfn "A" // prints "1/1326"
