let l = [0;1;2;3;4;5;6] in
let h::t = l in prInt h ;
let 1::t = t in
let 2::x::4::t = t in prInt x ;
let [a;b] = t in prInt (a+b) ;

let _a::_::b'::[] = (-1)::t in prInt (_a+b') 
