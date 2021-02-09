{
open Parser;;        (* le type "token" est défini dans parser.mli *)
(* ce n'est pas à vous d'écrire ce fichier, il est engendré automatiquement *)
exception Eof;;
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
| [' ' '\t']     { token lexbuf }    (* on saute les blancs et les tabulations *)
(* token: appel récursif *)
(* lexbuf: argument implicite
associé au tampon où sont
lus les caractères *)
| '\n'                                          { EOL }
| '+'                                           { PLUS }
| '*'                                           { TIMES }
| '-'                                           { MINUS }
| '='                                           { EQUAL }
| ">="                                          { GEQ }
| "<="                                          { LEQ }
| '>'                                           { GREATER }
| '<'                                           { LOWER }
| "||"                                          { OR }
| "&&"                                          { AND }
| '('                                           { LPAREN }
| ')'                                           { RPAREN }
| ['0'-'9']+ as s                               { INT (int_of_string s) }
| "let"                                         { LET }
| "in"                                          { IN }
| "not"                                         { NOT }
| "if"                                          { IF }
| "then"                                        { THEN }
| "else"                                        { ELSE }
| "true"                                        { TRUE }
| "false"                                       { FALSE }
| "prInt"                                       { PRINT }
| ['a'-'z']['0'-'9' 'a'-'z' 'A'-'Z']* as x      { VAR x }
| eof                                           { raise Eof }
