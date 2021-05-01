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

let reduc = ref false

let notypes = ref false
let showtypes = ref false
let showmoretypes = ref false
let monotypes = ref false

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
  ("-autotest", Arg.Set autotest, "Cette option, combinée avec l’option -cps, aura pour effet de comparer différentes exécutions du programme fourni en entrée");
  ("-optim", Arg.Set optim, "Cette option améliore la traduction CPS en appliquant des bêta-réductions");
  ("-reduc", Arg.Set reduc, "Prétraite le code en entrée en appliquant des bêta-réductions");
  ("-prefix", Arg.String (fun s -> Transformation.prefix := s), "Redéfinit le préfixe des variables introduites par l'interprète, par défaut \"x4V13r_L3r0y\"");
  ("-notypes", Arg.Set notypes, "Interprète les programme sans le typer au préalable");
  ("-showtypes", Arg.Set showtypes, "Affiche le type inféré pour toutes les déclarations en surface, l’expression principale étant désignée par \"-\"");
  ("-showmoretypes", Arg.Set showmoretypes, "Affiche les contraintes engendrées, puis comme showtype, puis la liste de tous les types inférés");
  ("-monotypes", Arg.Set monotypes, "Typage en version monomorphe (par défaut, version polymorphe")
]



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
  in (loop [])


let calc result =
  try
    if !tree then begin
      affiche_expr_tree result ; print_newline ()
    end ;

    if !debug || !showsrc then begin
      affiche_expr_code result ; print_newline ()
    end ;

    if !cps then begin
      let main_transform = if !optim then Reduction.reduction (Transformation.main_transform result) else Transformation.main_transform result in
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
        let v = compile StdLib.stdlib main_transform
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

  Arg.parse
    optlist

    (fun s -> nom_fichier := s) (* fonction a declencher lorsqu'on
                                 * recupere un string qui n'est pas une option *)
    ""; (* message d'accueil *)

  if !autotest && !cps then
    begin
      let in_from_stdin = if !std_input then read_stdin ()
        else "" in
      if 0 = Sys.command
           ("[ \"$(printf \"%b\n%b\n%b\" "
            ^ "\"let prInt i = print_int i; print_newline (); i\n;;\n\" "
            ^ "\"exception E of int\n;;\n\" "
            ^  (if !std_input then "\"" ^ in_from_stdin ^"\""
                else
                  "\"$(cat " ^ !nom_fichier ^ ")\""
               ) ^ " | ocaml -stdin)\" = \"$("
            ^  (if !std_input then "printf \"%b\n\" \""
                                   ^ in_from_stdin
                                   ^ "\" | ./fouine -stdin"
                else
                  "./fouine " ^ !nom_fichier
               ) ^ ")\" ]")
      then
        print_string "OK"
      else print_string "NO";
      print_newline ();
      if 0 = Sys.command
           ("[ \"$(printf \"%b\n%b\n%b\n%b\" "
            ^ "\"let prInt i = print_int i; print_newline (); i\n;;\n\" "
            ^  (if !std_input then
                  "\"$(printf \"%b\" \""
                  ^ in_from_stdin
                  ^ "\" | ./fouine -cps -optim -outcode -stdin)\""
                else
                  "\"$(./fouine -cps -optim -outcode " ^ !nom_fichier ^ ")\""
               ) ^ " | ocaml -stdin)\" = \"$("
            ^  (if !std_input then "printf \"%b\n\" \""
                                   ^ in_from_stdin
                                   ^ "\" | ./fouine -stdin"
                else
                  "./fouine " ^ !nom_fichier
               ) ^ ")\" ]")
      then
        print_string "OK"
      else print_string "NO";
      print_newline ();
      if 0 = Sys.command ("[ \"$("
                          ^ (if !std_input then
                               "\"$(printf \"%b\" \""
                               ^ in_from_stdin
                               ^ ")\" | ./fouine -cps -run -stdin)\""
                             else
                               "./fouine -cps -run " ^ !nom_fichier
                            ) ^ ")\" = \"$("
                          ^ (if !std_input then
                               "\"$(printf \"%b\" \""
                               ^ in_from_stdin
                               ^ ")\" | ./fouine -stdin)\""
                             else
                               "./fouine -run " ^ !nom_fichier
                            ) ^  ")\" ]")
      then
        print_string "OK"
      else print_string "NO";
      print_newline ()
    end
  else
    let lexbuf_file = Lexing.from_channel
        (if !std_input then stdin
         else open_in !nom_fichier)
    in

    let parse () = Parser.main Lexer.token lexbuf_file in
    try
      let result =
        let parsed = parse () in
        if !reduc then Reduction.reduction parsed else parsed
      in

      (* TYPAGE *)
      if not !notypes then begin
        try
          let pb, top_level =
            try
              (if !monotypes then Inference.inference  else Polymorphisme.inference )result
            with e -> print_string "erreur dans l'inf\n";
              raise e
          in

          if !showmoretypes then affiche_ct pb;

          let types =
            try
              Unification.unification pb
            with e -> print_string "erreur dans l'unif\n"; raise e
          in

          if !showtypes || !showmoretypes then (
            affiche_toplevel_types types top_level ;
            print_string ( "- : " ^ type_to_string (Types.find_type 0 types) ^ "\n") ;
          ) ;
          if !showmoretypes then affiche_type_list types ;

        with e -> print_string "Erreur de typage\n" ; raise e
      end ;

      calc result

    with
    | Unification.Not_unifyable -> print_string "Impossible d'unifier\n" ;
    | Unification.Cyclic_type pb -> print_string "Type cyclique\n" ; affiche_ct pb ;
    | e -> (* print_string "erreur de saisie\n" *)
      raise e

let _ = exec ()
