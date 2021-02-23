{
open Parser;;        (* le type "token" est d�fini dans parser.mli *)
(* ce n'est pas � vous d'�crire ce fichier, il est engendr� automatiquement *)
exception Eof;;
}

let ident = ['a'-'z']['0'-'9' 'a'-'z' 'A'-'Z' ''' '_']* 

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
| [' ' '\t' '\n']     	 			   		 	{ token lexbuf }
| '+'                                           { PLUS }
| '*'                                           { TIMES }
| '-'                                           { MINUS }
| '/'											{ DIV }
| "->"											{ MAPS }
| '='                                           { EQUAL }
| ">="                                          { GEQ }
| "<="                                          { LEQ }
| '>'                                           { GT }
| '<'                                           { LT }
| "||"                                          { OR }
| "&&"                                          { AND }
| '('                                           { LPAREN }
| ')'                                           { RPAREN }
| ['0'-'9']+ as s                               { INT (int_of_string s) }
| "let"                                         { LET }
| "in"                                          { IN }
| "fun"                  						{ FUN }
| "if"                                          { IF }
| "then"                                        { THEN }
| "else"                                        { ELSE }
| "true"                                        { TRUE }
| "false"                                       { FALSE }
| ident as x     								{ VAR x }
| eof                                           { EOF }
