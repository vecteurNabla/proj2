let rec f x =
  if x=0 then 1
  else x * f (x-1)
in

prInt (f 3) ;
prInt (f 5)
