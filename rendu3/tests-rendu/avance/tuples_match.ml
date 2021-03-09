let f x = x+42,x-42,x in
prInt begin
prInt begin
match f 1515 with
  | 1557, 0 -> 0
  | 1557,1473 -> 0
  | 1557,1473,0 -> 0
  | 1557,_ -> 0
  | 1557,1473,_ -> 1
end
+
prInt begin
match f 42 with
  | _,z -> z
end
end
