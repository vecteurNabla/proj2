
open Expr
open Affichage

let showsrc = ref false
let debug = ref false
let tree = ref false
let outval = ref false
let std_input = ref false

let compile e =
  let m = Memory.empty_mem () in
  let env = StdLib.load_stdlib [] in
  let k_init v = v in
  let k_break v =
    print_string "Exception : E " ;
    affiche_val v ;
    print_newline() ;
    VUnit
  in
  let v = eval env m e k_init k_break in
  if !outval then begin
    affiche_val v ; print_newline ()
  end


let calc result =
  try
    if !tree then begin
      affiche_expr_tree result ; print_newline ()
    end ;
    if !debug || !showsrc then begin
      affiche_expr_code result ; print_newline ()
    end ;
    if not !showsrc then begin
      compile result
    end ;
    flush stdout
  with
  | Unbound x -> print_string (x^" indéfinie\n")
  | Div_by_Zero -> print_string "erreur: division par zero\n"
  | App_not_fun -> print_string "erreur: une expression qui n'est pas une fonction ne peut pas être appliquée\n"
  | Not_expected s -> print_string ("erreur: " ^ s ^ " est attendu.e\n")
  | StdLib.PrInt_not_int -> print_string "erreur: argument de la fonction prInt n'est pas entier comme attendu\n"
  | Match_Failure -> print_string "erreur: matching impossible\n"


let exec () =

  let nom_fichier = ref "" in

  (* let lexbuf = Lexing.from_channel stdin in
   * let parse () = Parser.main Lexer.token lexbuf in *)

  let optlist = [
    ("-showsrc", Arg.Set showsrc, "Affiche le programme en entrée");
    ("-debug", Arg.Set debug, "Active le mode de debuggage : affiche le programme en entrée et les sorties du programme " );
    ("-tree", Arg.Set tree, "Affiche l'arbre du programme" );
    ("-outval", Arg.Set outval, "Affiche la valeur finale du programme" );
    ("-stdin", Arg.Set std_input, "Exécute le code depuis l'entrée standard (et pas depuis un fichier)" )
  ] in

  Arg.parse (* ci-dessous les 3 arguments de Arg.parse : *)
    optlist (* la liste des options definie ci-dessus *)

    (fun s -> nom_fichier := s) (* la fonction a declencher lorsqu'on recupere un string qui n'est pas une option : ici c'est le nom du fichier, et on stocke cette information dans la reference nom_fichier *)
    ""; (* le message d'accueil *)

  try
    let lexbuf_file = Lexing.from_channel
      (if !std_input then stdin
      else open_in !nom_fichier)
    in
    let parse () = Parser.main Lexer.token lexbuf_file in
    let result = parse () in
    calc result
  with _ -> print_string "erreur de saisie\n"


let _ = exec ()
