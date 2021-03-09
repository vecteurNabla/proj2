let rec f = function
  | x::t ->
    let g = function
      | (a,b) -> a+b
     in g x
  | [] -> 0
in prInt (f [(1,1);(2,2)])
