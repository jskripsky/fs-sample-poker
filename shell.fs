// Add these as ToString() overrides on `Rank` and `Suit`?

fsi.AddPrinter (function Jack -> "J" | Queen -> "Q" | King -> "K" | Ace -> "A" | Value x -> string x)
fsi.AddPrinter (function Spade -> "♠" | Heart -> "♥" | Diamond -> "♦" | Club -> "♣")

