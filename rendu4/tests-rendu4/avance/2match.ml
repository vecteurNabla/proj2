let f x = match x with
  | 0 -> prInt 42
  | 3 -> match (2*x) with
        | 6 -> prInt 237
        | 3 -> prInt 612
    in f 3
         
