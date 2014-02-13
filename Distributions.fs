module Distributions

/// Distribution infrastructure
type 'T Outcome = { Value: 'T; Probability: float }
type 'T Distribution = 'T Outcome seq
 
let bindD (dist:'T Distribution) (rest: 'T -> 'U Distribution) =
	dist
	|> Seq.map (fun o1 -> 
		o1.Value
		|> rest
		|> Seq.map (fun o2 -> { Value = o2.Value; Probability = o1.Probability * o2.Probability}))
	|> Seq.concat
 
let returnD value = Seq.singleton { Value = value ; Probability = 1.0 }

/// Monadic wrapper
type DistributionMonadBuilder () =
	member this.Bind (r, f) = bindD r f
	member this.Return x = returnD x
	member this.ReturnFrom m = m
 
let distribution = DistributionMonadBuilder ()

/// Utility functions
let toUniformDistribution seq =
	let l = Seq.length seq
	seq |> Seq.map (fun e -> { Value = e; Probability = 1.0 / (float l) })

let certainly = returnD
 
let probability dist =
	dist |> Seq.map (fun o -> o.Probability) |> Seq.sum
 
let filter predicate dist =
	dist |> Seq.filter (fun o -> predicate o.Value)
 
// Dice and coins
let fairDice sides = toUniformDistribution [1..sides]
type CoinSide = Heads | Tails
 
let fairCoin = toUniformDistribution [Heads; Tails]
