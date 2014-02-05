type ListMonad() =
	member o.Bind (m, f) = m |> List.map f |> List.concat
	member o.Return (x) = [x]

let list = ListMonad()

let sortedFullDeck =
	list {
		let! r = sortedRanks
		let! s = sortedSuits
		return (r, s) }


list.Bind (
	sortedRanks,
	fun r ->
		list.Bind (
			sortedSuits,
			fun s -> list.Return (r, s)))

let sortedFullDeck =
	sortedRanks
	|> List.map (
		fun r ->
			sortedSuits
			|> List.map (fun s -> [(r, s)])
			|> List.concat)
	|> List.concat

