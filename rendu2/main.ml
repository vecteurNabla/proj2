open Expr
open StdLib
open Memory

let compile e =
  begin
    affiche_expr e;
    print_newline();
    begin
      let m = empty_mem () in
      let env = load_stdlib [] in
      match eval env m e with
      | VInt x -> print_int x
      | _ -> print_string "valeur non imprimable"
    end ;
    print_newline()
  end

(* stdin d�signe l'entr�e standard (le clavier) *)
(* lexbuf est un canal ouvert sur stdin *)

let lexbuf = Lexing.from_channel stdin

(* on encha�ne les tuyaux: lexbuf est pass� � Lexer.token,
   et le r�sultat est donn� � Parser.main *)

let parse () = Parser.main Lexer.token lexbuf

(* la fonction que l'on lance ci-dessous *)
let calc () =
  try
      let result = parse () in
      (* Expr.affiche_expr result; print_newline (); flush stdout *)
	compile result; flush stdout
  with
  | Unbound x -> print_string (x^" ind�finie\n")
  | Div_by_Zero -> print_string "erreur: division par zero\n"
  | PrInt_not_int -> print_string "erreur: argument de la fonction prInt n'est pas entier comme attendu\n"
  | App_not_fun -> print_string "erreur: une expression qui n'est pas une fonction ne peut pas �tre appliqu�e\n"
  | Not_expected s -> print_string ("erreur: " ^ s ^ "est attendu.e\n")
  | _ -> (print_string "erreur de saisie\n")

let _ = calc()
