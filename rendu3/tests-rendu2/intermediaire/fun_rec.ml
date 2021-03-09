let rec f = fun x ->
  if x <= 0 then 0
  else x + f (x-1)
in prInt (f 3)
