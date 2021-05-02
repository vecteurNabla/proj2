let rec len x = match x with [] -> 0 | _::t -> 1 + len t
in len [], len [1;2;5;6], len [true]
