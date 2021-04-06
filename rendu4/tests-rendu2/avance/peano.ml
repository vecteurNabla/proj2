let succ l = () :: l
in

let rec int_to_peano = function
  | 0 -> []
  | n -> succ (int_to_peano (n-1))
in

let rec peano_to_int = function
  | [] -> 0
  | _::t -> 1 + peano_to_int t
in

let rec lt u v = match u,v with
  | _ , [] -> false
  | [] , _ -> true
  | _::s , _::t -> lt s t
in

let leq u v =
  u = v || lt u v
in

let gt u v = not ( lt u v )
in

let geq u v = not ( leq u v )
in


let rec add u = function
  | [] -> u
  | _::t -> succ (add u t)
in

let rec minus u v = match u,v with
  | [],_ -> []
  | _,[] -> u
  | _::s,_::t -> minus s t
in

let rec mult u = function
  | [] -> []
  | _::t -> add u (mult u t)
in

let rec division u v =
  if lt u v then [],u
  else
    let q,r = division (minus u v) v in
    succ q , r
in

let quot u v = fst (division u v)
in

let modulo u v = snd (division u v)
in


let rec base b u =
  if lt u b then [u]
  else
    let q,r = division u b in
    r::(base b q)
in

let u = int_to_peano 5 in
let v = int_to_peano 12 in
let w = mult u v in
let x = quot w [();();()] in
if geq x [] then prInt (peano_to_int x) else -1
