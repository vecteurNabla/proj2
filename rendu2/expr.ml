open Memory

(* type pour les variables *)
type var = string

(* un type pour des expressions *)
type expr =
    Const of int                  (* i *)
  | Add of expr*expr              (* + *)
  | Mul of expr*expr              (* * *)
  | Min of expr*expr              (* - *)
  | Div of expr*expr              (* + *)

  | Var of var                    (* x *)
  | Let of var*expr*expr          (* let x = e1 in e2 *)
  | Fun of var*expr               (* fun x -> e *)
  | FunRec of string*var*expr     (* rec fun x -> e *)
  | App of expr*expr              (* e1 e2 *)

  | If of ebool*expr*expr         (* if b then e1 else e2 *)

  | Unit                          (* () *)
  | Seq of expr * expr            (* e1; e2 *)
  | Aff of expr * expr            (* e1 := e2 *)
  | Der of expr                   (* !e *)

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
  | Neq of expr*expr

  | True
  | False

(* type pour les valeurs *)
type value =
  | VInt of int
  | VFun of var*env*expr
  | VRec of string*var*env*expr
  | VUnit
  | VRef of address
  (*| VBoo of bool*)
  | VStdLib of (env -> value mem -> expr -> value)
and env = (var*value) list


(* fonctions d'affichage *)
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
    | Neq(e1,e2) -> aff_aux "Neq(" e1 e2
    | True -> print_string "True"
    | False -> print_string "False"
  in
  match e with
  | Const k -> print_int k
  | Add(e1,e2) -> aff_aux "Add(" e1 e2
  | Mul(e1,e2) -> aff_aux "Mul(" e1 e2
  | Min(e1,e2) -> aff_aux "Min(" e1 e2
  | Div(e1,e2) -> aff_aux "Div(" e1 e2
  | Var x -> print_string x
  | Let(x,e1,e2) -> aff_aux ("Let(" ^ x  ^ ", ") e1 e2
  | Fun(x,e) -> begin
      print_string ("Fun(" ^ x ^ ", ") ;
      affiche_expr e ;
      print_string ")" ;
    end
  | FunRec(f,x,e) -> begin
      print_string ("FunRec(" ^ f ^ ", " ^ x ^ ", ") ;
      affiche_expr e ;
      print_string ")" ;
    end
  | App(e1,e2) -> aff_aux "App(" e1 e2 ;
  | If(b,e1,e2) -> begin
      print_string "If(" ;
      affiche_bool b ;
      aff_aux ", " e1 e2
    end
  | Unit -> print_string "Unit"
  | Seq(e1,e2) -> aff_aux "Seq(" e1 e2
  | Aff(e1,e2) -> aff_aux "Aff(" e1 e2
  | Der(e) -> begin
      print_string "Der(" ;
      affiche_expr e ;
      print_string ")"
    end


exception Div_by_Zero
exception App_not_fun
exception Not_expected of string
exception Unbound of string

let ( !. ) v = match v with
  | VInt(i) -> i
  | _ -> raise (Not_expected "un entier")

let ( !! ) v = match v with
  | VRef(a) -> a
  | _ -> raise (Not_expected "une référence")

let rec find x = function
  | [] -> raise (Unbound x)
  | h::_ when fst h = x -> snd h
  | _::t -> find x t

(* sémantique opérationnelle à grands pas *)
let rec eval env m = function
  | Const k -> VInt k
  | Add(e1,e2) -> VInt( !.(eval env m e1) + !.(eval env m e2) )
  | Mul(e1,e2) -> VInt( !.(eval env m e1) * !.(eval env m e2) )
  | Min(e1,e2) -> VInt( !.(eval env m e1) - !.(eval env m e2) )
  | Div(e1,e2) ->
    let deno = !.(eval env m e2) in (* On calcule d'abord le déno pour *)
    if deno = 0 then               (* ne pas avoir à calculer le num pour rien *)
      raise Div_by_Zero
    else
      VInt(!.(eval env m e1) / deno)
  | Var x -> find x env
  | Let(x,e1,e2) ->
    let v = eval env m e1 in
    eval ((x,v)::env) m e2
  | Fun(x,e) -> VFun(x,env,e)
  | FunRec(f,x,e) -> VRec(f, x, env, e)
  | App(e1,e2) -> begin
      let varg = eval env m e2 in
      let vfun = eval env m e1 in
      match vfun with
      | VFun(x,env',e) -> eval ((x,varg)::env') m e
      | VRec(f,x,env',e) -> eval ((x,varg)::(f,vfun)::env') m e
      | VStdLib(f) -> f env m e2 ;
      | _ -> raise App_not_fun
    end
  | If(b,e1,e2) -> if eval_bool env m b then eval env m e1 else eval env m e2
  | Unit -> VUnit
  | Seq(e1,e2) -> begin
      ignore (eval env m e1) ;
      eval env m e2
    end
  | Aff(e1, e2) -> begin
      let v = eval env m e2 in
      let a = !! (eval env m e1) in
      set_mem m a v ;
      VUnit
    end
  | Der(e) ->  let a = !! (eval env m e) in
    try get_mem m a
    with Not_found -> raise (Unbound "reference")
and eval_bool env m = function
  | And(a,b) -> (eval_bool env m a) && (eval_bool env m b)
  | Or(a,b) -> (eval_bool env m a) || (eval_bool env m b)
  | Not b -> not (eval_bool env m b)
  | Leq(e1,e2) -> (eval env m e1) <= (eval env m e2)
  | Geq(e1,e2) -> (eval env m e1) >= (eval env m e2)
  | Lt(e1,e2) -> (eval env m e1) < (eval env m e2)
  | Gt(e1,e2) -> (eval env m e1) > (eval env m e2)
  | Eq(e1,e2) -> (eval env m e1) = (eval env m e2)
  | Neq(e1,e2) -> (eval env m e1) <> (eval env m e2)
  | True -> true
  | False -> false
