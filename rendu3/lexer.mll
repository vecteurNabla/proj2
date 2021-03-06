{
open Parser;;        (* le type "token" est d�fini dans parser.mli *)
(* ce n'est pas � vous d'�crire ce fichier, il est engendr� automatiquement *)
exception Eof;;
}

let ident = ['a'-'z' '_']['0'-'9' 'a'-'z' 'A'-'Z' ''' '_']*

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
| [' ' '\t' '\n']                            { token lexbuf }
| "->"                                       { MAPS }
| '+'                                        { PLUS }
| '*'                                        { TIMES }
| '-'                                        { MINUS }
| '/'                                        { DIV }
| "<>"                                       { NEQ }
| '='                                        { EQUAL }
| ">="                                       { GEQ }
| "<="                                       { LEQ }
| '>'                                        { GT }
| '<'                                        { LT }
| "||"                                       { OR }
| "&&"                                       { AND }
| "()"                                       { UNIT }
| '('                                        { LPAREN }
| ')'                                        { RPAREN }
| ['0'-'9']+ as s                            { INT (int_of_string s) }
| "let"                                      { LET }
| "rec"                                      { REC }
| "in"                                       { IN }
| "match"                                    { MATCH }
| "with"                                     { WITH }
| "function"                                 { FUNCTION }
| "fun"                                      { FUN }
| "if"                                       { IF }
| "then"                                     { THEN }
| "else"                                     { ELSE }
| "true"                                     { BOOL true }
| "false"                                    { BOOL false }
| "begin"                                    { LPAREN }
| "end"                                      { RPAREN }
| "raise"									 { RAISE }
| "try"										 { TRY }
| 'E'										 { E }											 
| '|'                                        { PIPE }
| ";;"                                       { DBLSEMICOL }
| "::"                                       { CONS }
| "[]"                                       { NIL }
| '['                                        { LSQB }
| ']'                                        { RSQB }
| ';'                                        { SEQ }
| ','                                        { COMA }
| ":="                                       { AFF }
| '!'                                        { DER }
| '_'                                        { UNDER }
| ident as x                                 { VAR (Ident x) }
| eof                                        { EOF }
