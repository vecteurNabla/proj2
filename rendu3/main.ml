open Eval
open Affichage

(* options *)
let std_input = ref false

let showsrc = ref false
let debug = ref false
let tree = ref false

let outval = ref false

let cps = ref false
let outcode = ref false
let outcode_tree = ref false
let run = ref false
let autotest = ref false
let optim = ref false

let compile env e =
  let m = Memory.empty_mem () in
  let k_init v = v in
  let k_break v =
    print_string "Exception : E " ;
    affiche_val v ;
    print_newline() ;
    Expr.Const Expr.Unit
  in
  eval env m e k_init k_break

  
let read_stdin () =
  let try_read () =
    try
      Some (read_line () ^ "\n")
    with End_of_file -> None
  in
  let rec loop acc = match try_read () with
    | Some s -> loop (s::acc)
    | None -> List.fold_left (fun a b -> b ^ a) "" acc
  in String.escaped (loop [])

   
let calc result =
  try
    if !tree then begin
      affiche_expr_tree result ; print_newline ()
    end ;

    if !debug || !showsrc then begin
      affiche_expr_code result ; print_newline ()
    end ;

    if !cps then begin
      let main_transform = Transformation.transform result in
        if !outcode then begin
          affiche_expr_code main_transform ; print_newline ()
        end ;

        if !outcode_tree then begin
          affiche_expr_tree main_transform ; print_newline ()
        end ;

        if !autotest then begin
          ()
        end ;

        if !run then begin
          let v = compile StdLib.stdlib_transform (Transformation.arg_init main_transform)
          in
          if !outval then begin
            affiche_val v ; print_newline ()
          end
        end
    end

    else if not !showsrc then begin
      let v = compile StdLib.stdlib result in
      if !outval then begin
        affiche_val v ; print_newline ()
      end
    end ;

    flush stdout

  with
  | Unbound x -> print_string (x ^ " indéfinie\n")
  | Div_by_Zero -> print_string "erreur: division par zero\n"
  | App_not_fun (e,v) -> print_string "erreur: l'expression suivante n'est pas une fonction, elle ne peut pas être appliquée\n |  " ;
    affiche_expr_code e ; print_string " = " ; affiche_val v ; print_newline () ;
  | Not_expected s -> print_string ("erreur: " ^ s ^ " est attendu.e\n")
  | StdLib.PrInt_not_int -> print_string "erreur: argument de la fonction prInt n'est pas entier comme attendu\n"
  | Match_Failure s -> print_string ( "erreur: matching impossible" ^ s ^ "\n")


let exec () =

  let nom_fichier = ref "" in

  (* let lexbuf = Lexing.from_channel stdin in
   * let parse () = Parser.main Lexer.token lexbuf in *)

  let optlist = [
    ("-showsrc", Arg.Set showsrc, "Affiche le programme en entrée");
    ("-debug", Arg.Set debug, "Active le mode de debuggage : affiche le programme en entrée et les sorties du programme " );
    ("-tree", Arg.Set tree, "Affiche l'arbre du programme" );
    ("-outval", Arg.Set outval, "Affiche la valeur finale du programme" );
    ("-stdin", Arg.Set std_input, "Exécute le code depuis l'entrée standard (et pas depuis un fichier)" );
    ("-cps", Arg.Set cps, "Applique la transformation pour éliminer les exceptions");
    ("-outcode", Arg.Set outcode, "Cette option, combinée avec l’option -cps, aura pour effet d’afficher à l’écran le programme résultant de la transformation, sans l’exécuter");
    ("-outcode-tree", Arg.Set outcode_tree, "Cette option, combinée avec l’option -cps, aura pour effet d’afficher à l’écran le programme résultant de la transformation sous forme d'arbre, sans l’exécuter");
    ("-run", Arg.Set run, "Cette option, combinée avec l’option -cps, aura pour effet d’exécuter avec fouine le programme résultant de la traduction");
    ("-autotest", Arg.Set autotest, "Cette option, combinée avec l’option -cps, aura pour effet de comparer différentes exécutions du programme fourni en entrée [OPTION NON PRISE EN CHARGE A L'HEURE ACTUELLE]");
    ("-optim", Arg.Set optim, "Cette option améliore la traduction CPS en appliquant les simplifications décrites dans les notes de cours [OPTION NON PRISE EN CHARGE A L'HEURE ACTUELLE]")
  ] in

  Arg.parse
    optlist

    (fun s -> nom_fichier := s) (* fonction a declencher lorsqu'on
                             * recupere un string qui n'est pas une option *)
    ""; (* message d'accueil *)

  try
    let lexbuf_file = Lexing.from_channel
      (if !std_input then stdin
      else open_in !nom_fichier)
    in
    if !autotest then
      begin
        (* let in_from_stdin = if !std_input then read_stdin ()
         *                     else "" in
         * if 0 = (Sys.command
         *           ("[ `echo 'let prInt = print_int;;' | cat - "
         *            ^ (if not !std_input then !nom_fichier else
         *                 "<(echo \"" ^ in_from_stdin ^ "\" )"
         *              )
         *            ^ " | ocaml -stdin` = `./fouine "
         *            ^ (if not !std_input then !nom_fichier else
         *                 "-stdin <(echo \"" ^ in_from_stdin ^ "\" )"
         *              )
         *            ^ "` ]")) then
         *   print_string "OK"
         * else print_string "NO";
         * print_newline () *)
        print_string ("echo \"" ^ read_stdin () ^ "\"")
      end
    else
      let parse () = Parser.main Lexer.token lexbuf_file in
      let result = parse () in
      calc result
  with _ -> print_string "erreur de saisie\n"


let _ = exec ()
