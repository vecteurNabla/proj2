let g x =
  let y = x + 1 in
  let z = y <= x * x - 6 in
  let t = if z then fun (x,y) -> x else fun (x,y) -> [y] in
  t ([],x)
in
g (-1)
