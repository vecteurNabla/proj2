let rec f x = if x = 0 then -1 else (f (x+1)) + 1
in f 42
