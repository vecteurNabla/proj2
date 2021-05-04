try
  let x = raise (E 0) in
  raise (E 2) ; 5
with E x -> x
