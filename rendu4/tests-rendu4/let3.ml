let f x y = (fun z -> [x ; y (z<1)]) in
let g = fun x -> x in
f true g 1515 , f [] (fun b -> [b]) (-1)
