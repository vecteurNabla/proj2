let f x =
  let a = 42 in
  a + x
;;

let rec fact n = match n with
  | 0 -> 1
  | _ -> n * (fact (n-1))
;;

prInt (f (fact 5))
