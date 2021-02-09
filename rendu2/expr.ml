(* type pour les variables *)
type var = string

(* un type pour des expressions arithmétiques simples *)
type expr =
    Const of int
  | Add of expr*expr
  | Mul of expr*expr
  | Min of expr*expr
  | Var of var
  | Let of var*expr*expr
  | If of ebool*expr*expr
  | Print of expr

(* type pour les expressions booléennes *)
and ebool =
  | And of ebool*ebool
  | Or of ebool*ebool
  | Not of ebool
  | Leq of expr*expr
  | Geq of expr*expr
  | Lt of expr*expr
  | Gt of expr*expr
  | Eq of expr*expr
  | True
  | False

(* type pour les valeurs *)
type value = int


(* fonctions d'affichage *)
let ($) s1 s2 = String.concat "" [s1;s2]

let rec affiche_expr e =
  let aff_aux s a b = 
      begin
	print_string s;
	affiche_expr a;
	print_string ", ";
	affiche_expr b;
	print_string ")"
      end
  in
  let rec affiche_bool = function
    | And(a,b) -> begin
        print_string "And(" ;
        affiche_bool a ;
        print_string ", " ;
        affiche_bool b ;
        print_string ")"
      end
    | Or(a,b) -> begin
        print_string "Or(" ;
        affiche_bool a ;
        print_string ", " ;
        affiche_bool b ;
        print_string ")"
      end
    | Not b -> begin
        print_string "Not " ;
        affiche_bool b ;
      end
    | Leq(e1,e2) -> aff_aux "Leq(" e1 e2
    | Geq(e1,e2) -> aff_aux "Geq(" e1 e2
    | Lt(e1,e2) -> aff_aux "Lt(" e1 e2
    | Gt(e1,e2) -> aff_aux "Gt(" e1 e2
    | Eq(e1,e2) -> aff_aux "Eq(" e1 e2
    | True -> print_string "True"
    | False -> print_string "False"
  in
  match e with
  | Const k -> print_int k
  | Add(e1,e2) -> aff_aux "Add(" e1 e2
  | Mul(e1,e2) -> aff_aux "Mul(" e1 e2
  | Min(e1,e2) -> aff_aux "Min(" e1 e2
  | Var x -> print_string x
  | Let(x,e1,e2) -> aff_aux ("Let(" $ x $ ", ") e1 e2
  | If(b,e1,e2) -> begin
      print_string "If(" ;
      affiche_bool b ;
      aff_aux ", " e1 e2
    end
  | Print e -> begin
      print_string "PrInt(" ;
      affiche_expr e ;
      print_string")" ;
    end


exception Free_var of string

let rec find x = function
  | [] -> raise (Free_var x)
  | h::_ when fst h = x -> snd h
  | _::t -> find x t

(* sémantique opérationnelle à grands pas *)
let rec eval env = function
  | Const k -> k
  | Add(e1,e2) -> (eval env e1) + (eval env e2)
  | Mul(e1,e2) -> (eval env e1) * (eval env e2)
  | Min(e1,e2) -> (eval env e1) - (eval env e2)
  | Var x -> find x env
  | Let(x,e1,e2) ->
    let v = eval env e1 in
    eval ((x,v)::env) e2
  | If(b,e1,e2) -> if eval_bool env b then eval env e1 else eval env e2
  | Print e -> begin
    let v = eval env e in
    print_int v ;
    print_newline () ;
    v
  end
and eval_bool env = function
  | And(a,b) -> (eval_bool env a) && (eval_bool env b)
  | Or(a,b) -> (eval_bool env a) || (eval_bool env b)
  | Not b -> not (eval_bool env b)
  | Leq(e1,e2) -> (eval env e1) <= (eval env e2)
  | Geq(e1,e2) -> (eval env e1) >= (eval env e2)
  | Lt(e1,e2) -> (eval env e1) < (eval env e2)
  | Gt(e1,e2) -> (eval env e1) > (eval env e2)
  | Eq(e1,e2) -> (eval env e1) = (eval env e2)
  | True -> true
  | False -> false
