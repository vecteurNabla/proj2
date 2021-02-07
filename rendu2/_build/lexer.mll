{
open Parser;;        (* le type "token" est d�fini dans parser.mli *)
(* ce n'est pas � vous d'�crire ce fichier, il est engendr� automatiquement *)
exception Eof;;
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
| [' ' '\t']     { token lexbuf }    (* on saute les blancs et les tabulations *)
(* token: appel r�cursif *)
(* lexbuf: argument implicite
associ� au tampon o� sont
lus les caract�res *)
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
| ['a'-'z']['0'-'9' 'a'-'z' 'A'-'Z']* as x      { VAR x }
| eof                                           { raise Eof }
