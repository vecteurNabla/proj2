let x = 3 in
prInt begin
match x with
| 3 -> let x = x+42 in
match 4 with 3 -> 0
| _ -> x
end
