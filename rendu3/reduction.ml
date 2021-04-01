open Expr
open Eval

(* trouve dans la liste l'expression correspondant au nom : expr *)
let rec find_var x = function
  | [] -> raise (Unbound x)
  | h::_ when fst h = x -> snd h
  | _::t -> find_var x t

(* trouve dans la liste les expressions correspondant au pattern : expr *)
let rec find_pattern rl = function
  | Under -> raise (Not_expected "une variable")
  | Ident x -> find_var x rl
  | PConst c -> ~& c
  | PCpl (x1,x2) -> Cpl(find_pattern rl x1, find_pattern rl x2)
  | PList (h,t) -> Cons(find_pattern rl h, find_pattern rl t)

(* ajoute a la list rl le pattern et les expressions irreductibles associées : list *)
let rec add_pattern rl x e = match x,e with
  | Under, _ -> rl
  | Ident x, _ -> (x,e)::rl
  | PConst c, Val(Const c') when c=c' -> rl
  | PCpl (x,y), Cpl(e1,e2) -> add_pattern (add_pattern rl x e1) y e2
  | PList (h,t), Cons(e1,e2) -> add_pattern (add_pattern rl h e1) t e2
  | _ -> raise (Match_Failure "NOT_IRR")

(* ajoute a la list rl le pattern et les expressions irreductibles associées : list *)
let rec remove_pattern rl x = match x with
  | Ident x ->
    let rec del = function
        [] -> []
      | (x',_)::t when x'=x -> del t
      | h::t -> h :: del t
    in del rl
  | PCpl (x,y) -> remove_pattern (remove_pattern rl x) y
  | PList (h,t) -> remove_pattern (remove_pattern rl h) t
  | _ -> rl


let rec reduc chg rl e = match e with
  (* on fait la reduc chg dans ce cas si e2 est irredutible *)
  | App( Fun(x,e') , e2 ) when irreductible e2 ->
    (try
       let r = reduc chg (add_pattern rl x (reduc chg rl e2)) e' in
       chg := true ; r
    with Match_Failure "NOT_IRR" ->
      App( Fun(x, reduc chg (remove_pattern rl x) e'), reduc chg rl e2 )
   )

  (* on remplace dans ce cas *)
  | Pattern x ->
    (try find_pattern rl x
    with Unbound _ | Not_expected _ -> e)

  (* on propage simplement *)
  | Val _  -> e

  | Let(x, e1, e2) ->
    Let(x, reduc chg (remove_pattern rl x) e1, reduc chg rl e2)
  | Fun(x,e) -> Fun(x, reduc chg (remove_pattern rl x) e)
  | Rec(x, e1, e2) -> Rec(x, reduc chg (remove_pattern rl x) e1, reduc chg rl e2)
  | App(e1, e2) -> App(reduc chg rl e1, reduc chg rl e2)

  | Cpl(e1, e2) -> Cpl(reduc chg rl e1, reduc chg rl e2)

  | Seq(e1, e2) -> Seq(reduc chg rl e1, reduc chg rl e2)
  | Aff(e1, e2) -> Aff(reduc chg rl e1, reduc chg rl e2)
  | Der(e) -> Der(reduc chg rl e)

  | If(b, e1, e2) -> If(reduc chg rl b, reduc chg rl e1, reduc chg rl e2)

  | Match(e, l) -> Match(reduc chg rl e, List.map (fun (x,e') -> x,reduc chg (remove_pattern rl x) e') l)

  | Cons(e1, e2) -> Cons(reduc chg rl e1, reduc chg rl e2)

  | Try(e1,x,e2) -> Try(reduc chg rl e1, x, reduc chg (remove_pattern rl x) e2)
  | Raise(e) -> Raise(reduc chg rl e)


let reduction e =
  let change = ref false in
  let r = ref (reduc change [] e) in
  while !change do
    change := false ;
    r := reduc change [] !r
  done ;
  !r
