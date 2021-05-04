try
  let x = raise (E []) in
  raise (E 2) ; 5
with E x -> x
