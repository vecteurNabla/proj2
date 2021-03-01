open Memory

(* type pour les variables *)
type var = Some of string | None | Couple of var*var

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
  | Rec of var*expr*expr          (* let rec f x = e in e *)
  | App of expr*expr              (* e1 e2 *)

  | Cpl of expr*expr

  | Unit                          (* () *)
  | Seq of expr * expr            (* e1; e2 *)
  | Aff of expr * expr            (* e1 := e2 *)
  | Der of expr                   (* !e *)

  | If of expr*expr*expr         (* if b then e1 else e2 *)

  | And of expr*expr
  | Or of expr*expr
  | Not of expr

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
  | VBoo of bool
  | VStdLib of (value mem -> value -> value)
  | VCpl of value*value
and env = (string*value) list


(* fonctions d'affichage *)
let rec var_to_string = function
  | None -> "_"
  | Some x -> x
  | Couple (x,y) -> "(" ^ var_to_string x ^ "," ^ var_to_string y ^")"

let rec affiche_expr_code e =
  let aff_aux s1 a s2 b s3 =
      begin
	print_string s1;
	affiche_expr_code a;
	print_string s2;
	affiche_expr_code b;
	print_string s3
      end
  in
  match e with
  | Const k -> print_int k
  | Add(e1,e2) -> aff_aux "(" e1 " + " e2 ")"
  | Mul(e1,e2) -> aff_aux "(" e1 " * " e2 ")"
  | Min(e1,e2) -> aff_aux "(" e1 " - " e2 ")"
  | Div(e1,e2) -> aff_aux "(" e1 " / " e2 ")"

  | Var x -> print_string (var_to_string x)
  | Let(x,e1,e2) -> aff_aux ("let " ^ (var_to_string x)  ^ " = ") e1 " in " e2 ""
  | Fun(x,e) -> aff_aux "fun " (Var x) " -> " e ""
  | Rec(f,e1, e2) -> aff_aux ("let rec " ^ (var_to_string f)  ^ " = " ) e1 " in " e2 ""
  | App(e1,e2) -> aff_aux "" e1 " (" e2 ")"

  | Cpl(e1,e2) -> aff_aux "(" e1 "," e2 ")"

  | Unit -> print_string "()"
  | Seq(e1,e2) -> aff_aux "" e1 " ; " e2 ""
  | Aff(e1,e2) -> aff_aux "" e1 " := " e2 ""
  | Der(e) -> begin
      print_string "!(" ;
      affiche_expr_code e ;
      print_string ")"
    end

  | If(b,e1,e2) -> begin
      aff_aux "if " b " then " e1 " else " ;
      affiche_expr_code e2
    end
  | And(a,b) -> aff_aux "(" a " && " b ")"
  | Or(a,b) -> aff_aux "(" a " || " b ")"
  | Not b -> begin
      print_string "not " ;
      affiche_expr_code b ;
    end
  | Leq(e1,e2) -> aff_aux "" e1 "<=" e2 ""
  | Geq(e1,e2) -> aff_aux "" e1 ">=" e2 ""
  | Lt(e1,e2) -> aff_aux "" e1 "<" e2 ""
  | Gt(e1,e2) -> aff_aux "" e1 ">" e2 ""
  | Eq(e1,e2) -> aff_aux "" e1 "=" e2 ""
  | Neq(e1,e2) -> aff_aux "" e1 "<>" e2 ""
  | True -> print_string "True"
  | False -> print_string "False"

let rec affiche_expr_tree e =
  let aff_aux s a b =
      begin
	print_string s;
	affiche_expr_tree a;
	print_string ", ";
	affiche_expr_tree b;
	print_string ")"
      end
  in
  match e with
  | Const k -> print_int k
  | Add(e1,e2) -> aff_aux "Add(" e1 e2
  | Mul(e1,e2) -> aff_aux "Mul(" e1 e2
  | Min(e1,e2) -> aff_aux "Min(" e1 e2
  | Div(e1,e2) -> aff_aux "Div(" e1 e2

  | Var x -> print_string (var_to_string x)
  | Let(x,e1,e2) -> aff_aux ("Let(" ^ (var_to_string x)  ^ ", ") e1 e2
  | Fun(x,e) -> begin
      print_string ("Fun(" ^ (var_to_string x) ^ ", ") ;
      affiche_expr_tree e ;
      print_string ")" ;
    end
  | Rec(f,e1, e2) -> begin
      aff_aux ("Rec(" ^ (var_to_string f) ^  ", ") e1 e2
    end
  | App(e1,e2) -> aff_aux "App(" e1 e2

  | Cpl(e1,e2) -> aff_aux "Cpl(" e1 e2

  | Unit -> print_string "Unit"
  | Seq(e1,e2) -> aff_aux "Seq(" e1 e2
  | Aff(e1,e2) -> aff_aux "Aff(" e1 e2
  | Der(e) -> begin
      print_string "Der(" ;
      affiche_expr_tree e ;
      print_string ")"
    end

  | If(b,e1,e2) -> begin
      print_string "If(" ;
      affiche_expr_tree b ;
      aff_aux ", " e1 e2
    end
  | And(a,b) -> aff_aux "And(" a b ;
  | Or(a,b) -> aff_aux "Or(" a b
  | Not b -> begin
      print_string "Not(" ;
      affiche_expr_tree b ;
      print_string ")"
    end
  | Leq(e1,e2) -> aff_aux "Leq(" e1 e2
  | Geq(e1,e2) -> aff_aux "Geq(" e1 e2
  | Lt(e1,e2) -> aff_aux "Lt(" e1 e2
  | Gt(e1,e2) -> aff_aux "Gt(" e1 e2
  | Eq(e1,e2) -> aff_aux "Eq(" e1 e2
  | Neq(e1,e2) -> aff_aux "Neq(" e1 e2
  | True -> print_string "True"
  | False -> print_string "False"

let rec affiche_val = function
  | VInt x -> print_int x
  | VFun _ -> print_string "<fun>"
  | VRec _ -> print_string "<fun rec>"
  | VStdLib _ -> print_string "<fun stdlib>"
  | VUnit -> print_string "()"
  | VRef _ -> print_string "<address>"
  | VBoo true -> print_string "True"
  | VBoo false -> print_string "False"
  | VCpl (v1,v2) ->
    print_string "(" ; affiche_val v1 ; print_string "," ; affiche_val v2 ; print_string ")"

exception Div_by_Zero
exception App_not_fun
exception Not_expected of string
exception Unbound of string

let ( !. ) v = match v with
  | VInt(i) -> i
  | _ -> raise (Not_expected "un entier")

let ( !? ) v = match v with
  | VBoo(a) -> a
  | _ -> raise (Not_expected "une booleen")

let ( !! ) v = match v with
  | VRef(a) -> a
  | _ -> raise (Not_expected "une reference")

let ( !& ) v = match v with
  | VCpl (v1,v2) -> v1
  | _ -> raise (Not_expected "un tuple" )

let ( !&& ) v = match v with
  | VCpl (v1,v2) -> v2
  | _ -> raise (Not_expected "un tuple" )

let rec find_var_name x = function
  | [] -> raise (Unbound x)
  | h::_ when fst h = x -> snd h
  | _::t -> find_var_name x t

let rec find env = function
  | None -> raise (Not_expected "une variable")
  | Some x -> find_var_name x env
  | Couple (x1,x2) -> VCpl(find env x1, find env x2)

let rec add_var_to_env env x v = match x with
  | None -> env
  | Some x -> (x,v)::env
  | Couple(x,y) -> add_var_to_env (add_var_to_env env x !&v) y !&&v

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

  | Cpl(e1,e2) -> VCpl(eval env m e1, eval env m e2)

  | Var x -> find env x

  | Let(x, e1, e2) ->
    let v = eval env m e1 in
    let env' = add_var_to_env env x v in
    eval env' m e2

  | Fun(x,e) -> VFun(x,env,e)
  | Rec(Some f,e1,e2) -> begin
      match e1 with
      | Fun(x,e) ->
        let v = VRec(f, x, env, e) in
        eval ((f,v)::env) m e2
      | _ -> raise (Not_expected "une fonction")
    end
  | Rec(_, _, _) -> raise (Not_expected "un nom de fonction recursive")
  | App(e1,e2) -> begin
      let varg = eval env m e2 in
      let vfun = eval env m e1 in
      match vfun with
      | VFun(x,env',e)   -> eval (add_var_to_env env' x varg) m e
      | VRec(f,x,env',e) -> eval (add_var_to_env ((f,vfun)::env') x varg) m e
      | VStdLib(f) -> f m varg ;
      | _ -> raise App_not_fun
    end

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
  | Der(e) ->  begin
      let a = !! (eval env m e) in
      try get_mem m a
      with Not_found -> raise (Unbound "reference")
    end
  | If(b,e1,e2) -> if !?(eval env m b) then eval env m e1 else eval env m e2
  | And(a,b) -> VBoo ( !?(eval env m a) && !?(eval env m b) )
  | Or(a,b) -> VBoo ( !?(eval env m a) || !?(eval env m b) )
  | Not b -> VBoo ( not (!?(eval env m b)) )
  | Leq(e1,e2) -> VBoo ( !.(eval env m e1) <= !.(eval env m e2) )
  | Geq(e1,e2) -> VBoo ( !.(eval env m e1) >= !.(eval env m e2) )
  | Lt(e1,e2) -> VBoo ( !.(eval env m e1) < !.(eval env m e2) )
  | Gt(e1,e2) -> VBoo ( !.(eval env m e1) > !.(eval env m e2) )
  | Eq(e1,e2) -> VBoo ( !.(eval env m e1) = !.(eval env m e2) )
  | Neq(e1,e2) -> VBoo ( !.(eval env m e1) <> !.(eval env m e2) )
  | True -> VBoo true
  | False -> VBoo false
