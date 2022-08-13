(* We're assuming that n > 0 *)
let round_away_from_zero n = function
  | x when x mod n = 0 -> x
  | x when x < 0 ->
      (* when x is negative and n is positive, x mod n will be negative *)
      x - n - (x mod n)
  | x -> x + n - (x mod n)
