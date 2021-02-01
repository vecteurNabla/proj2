open Cell
open Sheet
open Command

(* Si on lit depuis un fichier, le nom du fichier *)
let file_name = ref ""

(* Lit-on depuis un fichier ? *)
let from_file = ref false
           

(*** début de la partie "incantatoire" ***)
(* stdin désigne l'entrée standard (le clavier) *)
(* lexbuf est un canal ouvert sur stdin *)
let lexbuf () = Lexing.from_channel
               (if !from_file then
                  (open_in !file_name)
                else stdin)

(* on enchaîne les tuyaux: lexbuf est passé à Lexer.token,
   et le résultat est donné à Parser.main *)

let parse () = Parser.debut Lexer.token (lexbuf ())
 (*** fin de la partie "incantatoire" ***)

let spreadsheet () =
  (* Les différentes options de l'exécutable *)
  let optlist = [
      ("-paf", Arg.Set paf, "Active le mode paf");
      ("-debug", Arg.Set Debug.debug, "Active le mode de debuggage")
    ] in
  let usage = "Ze Okamel spraidechite" in
  Arg.parse
    optlist
    (fun s ->
      from_file := true;
      file_name := s)
    usage;
  try  
    let result = parse () in
    (
      (* le seul endroit a comprendre (dans un premier temps) :
         appel a la fonction run_script, qui est definie dans command.ml *)
      run_script result;
      flush stdout;
    )
  with Paf -> (print_string "\nPAF\n")
     | x -> if !Debug.debug then raise x else
             (print_string "Erreur de saisie\n")
;;


let _ = spreadsheet()

(* let _ = let scr = [ Upd (("C",2), Cst 3.7); ShowAll ] in
 *         begin
 *           print_string "HAHAHA!!!\n";
 *           run_script scr
 *         end *)
