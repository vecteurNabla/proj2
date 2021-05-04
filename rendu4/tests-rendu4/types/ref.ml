let f = ref (fun z -> z) in
begin
  f := (fun x -> x+1) ;
  !f 0
end
, ref 0
