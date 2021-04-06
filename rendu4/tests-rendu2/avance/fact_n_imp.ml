let c = ref 0 in
let rec f x =
  if x<=0 then 1
  else (c := !c+1 ; x * f (x-1) )
in
prInt (f 5) ;
prInt !c
