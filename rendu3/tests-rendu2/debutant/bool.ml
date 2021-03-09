let a = true in
let b = false in

if a && b then prInt 0 else prInt 1 ;
if a || b then prInt 1 else prInt 0 ;
if a && not b then prInt 1 else prInt 0 ;
if not a || b then prInt 0 else prInt 1

