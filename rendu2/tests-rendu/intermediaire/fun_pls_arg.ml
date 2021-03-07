let a = fun x y z -> x && (y || z) in
if a true true false then prInt 1 else prInt 0 ;

let b x y z = x && (y || z) in
if b true false true then prInt 1 else prInt 0
