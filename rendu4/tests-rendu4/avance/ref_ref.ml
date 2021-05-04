let x = ref ref in
let f x = x + 1515 in
let g x = x * 1515 in

let b = !x true in
x := f ;
b := !x 42 > 2021 ;
b := begin
  x := not ;
  if !x !b then prInt 1 else prInt 0 ;
  !b || not (x := g ; 2021 <= !x (x := prInt ; !x 42) )
end;
if !b then prInt 1 else prInt 0
