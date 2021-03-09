type address = int

type 'a mem = (address, 'a) Hashtbl.t

let empty_mem () = Hashtbl.create 612

let get_mem = Hashtbl.find

let set_mem = Hashtbl.add

let alloc_mem m x =
  let a = Hashtbl.length m in
  set_mem m a x; a

