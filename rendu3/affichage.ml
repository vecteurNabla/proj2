open Expr

(* fonctions d'affichage *)
let const_to_string = function
  | Int i -> string_of_int i
  | Bool true -> "true"
  | Bool false -> "false"
  | Unit -> "()"
  | Nil -> "[]"

let rec pattern_to_string = function
  | Under -> "_"
  | Ident x -> x
  | PConst c -> const_to_string c
  | PCpl (x,y) -> "(" ^ pattern_to_string x ^ "," ^ pattern_to_string y ^")"
  | PList (x,y) -> pattern_to_string x ^ "::" ^ pattern_to_string y

let rec affiche_val = function
  | Const c -> print_string (const_to_string c)
  | Ref _ -> print_string "<address>"
  | StdLib _ -> print_string "<fun stdlib>"
  | VFun (x,env,e) ->
    print_string ("<fun> : fun " ^ pattern_to_string x ^ " -> ") ;
    affiche_expr_code e
  | VCpl (v1,v2) ->
    print_string "(" ; affiche_val v1 ; print_string "," ; affiche_val v2 ; print_string ")"
  | VList (v1,v2) ->
    print_string "(" ; affiche_val v1 ; print_string "::" ; affiche_val v2 ; print_string ")"

and affiche_expr_code e =
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
  | Val v -> affiche_val v

  | Pattern x -> print_string (pattern_to_string x)
  | Let(x,e1 ,e2) -> aff_aux ("let " ^ (pattern_to_string x)  ^ " = ")
                      e1 " in " e2 ""
  | Fun(x,e) -> aff_aux "(fun " (Pattern x) " -> " e ")"
  | Rec(x, e, e') -> aff_aux ("let rec " ^ (pattern_to_string x) ^ " = " ) e " in " e' ""

  | App(Fun _ as e1, e2) -> aff_aux "(" e1 ") (" e2 ")"
  | App(e1,e2) -> aff_aux "" e1 " (" e2 ")"

  | Cpl(e1,e2) -> aff_aux "(" e1 "," e2 ")"

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

  | Match(e,l) -> begin
      print_string "match " ;
      affiche_expr_code e ;
      print_string " with " ;
      List.iter
        (fun (x, e) ->
           print_string ("| " ^ pattern_to_string x ^ " -> " );
           affiche_expr_code e ;
           print_string " " ;
        )
        l
    end

  | Cons(e1,e2) -> aff_aux "(" e1 ")::(" e2 ")"

  | Try(e1,x,e2) -> aff_aux "try " e1 (" with E " ^ pattern_to_string x ^ " -> ") e2 ""

  | Raise e -> begin
      print_string "raise (E " ;
      affiche_expr_code e ;
      print_string ")"
    end

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
  | Val v -> affiche_val v

  | Pattern x -> print_string (pattern_to_string x)
  | Let(x,e1,e2) -> aff_aux ("Let(" ^ (pattern_to_string x)  ^ ", ") e1 e2
  | Fun(x,e) -> begin
      print_string ("Fun(" ^ (pattern_to_string x) ^ ", ") ;
      affiche_expr_tree e ;
      print_string ")" ;
    end
  | Rec(f,e,e') -> aff_aux ("Rec(" ^ (pattern_to_string f) ^ ", ") e e'
  | App(e1,e2) -> aff_aux "App(" e1 e2

  | Cpl(e1,e2) -> aff_aux "Cpl(" e1 e2

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

  | Match(e,l) -> begin
      print_string "Match(" ;
      affiche_expr_tree e ;
      print_string ", " ;
      List.iter
        (fun (x, e) ->
           print_string ("(" ^ pattern_to_string x ^ ", " );
           affiche_expr_tree e ;
           print_string ")"
        )
        l ;
      print_string ")"
    end

  | Cons(e1,e2) -> aff_aux "Cons(" e1 e2

  | Try(e1,p,e2) -> begin
      print_string "Try(" ;
      affiche_expr_tree e1 ;
      print_string (", "^ pattern_to_string p ^ ", ") ;
      affiche_expr_tree e2 ;
      print_string ")"
    end

  | Raise e -> begin
      print_string "Raise(" ;
      affiche_expr_code e ;
      print_string ")"
    end

