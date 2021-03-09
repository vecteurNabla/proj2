let eval x f = f x in
let f x = prInt (x*x) in
let g = not in
eval 2 f ;
if eval true g then prInt 1 else prInt 0
