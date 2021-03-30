try
  let f = fun x -> 42*x + 1515 in
  let x = 1 in
  if f x > 0 then
    raise (E [1;2;5])
  else prInt f x
with E x::_ -> x
