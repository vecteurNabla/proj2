let a = 1515 in
let b = 42 in

let x1 = a > b in
let x2 = a <> b in
let x3 = b < a in

if x1 && x2 && x3 then prInt 1 else prInt 0 ;


let x4 = a = 1515 in
let x5 = 42 = b in
let x6 = b <= 42 in
let x7 = 1515 >= a in

if x4 && x5 && x6 && x7 then prInt 1 else prInt 0
