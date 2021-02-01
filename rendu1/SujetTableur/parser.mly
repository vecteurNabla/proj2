%{
(* --- préambule: ici du code Caml --- *)

open Cell
open Command

%}
/* énumération des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */

%token <int> INT       /* le lexème INT a un attribut entier */
%token <float> NBR       /* le lexème NBR a un attribut flottant */
%token <string> CELLROW       /* le lexème CELLROW a un attribut, de type string */
%token SHEET				  /* char 's', pour signifier qu'on change de table */
%token LPAREN RPAREN EQUAL SEMICOL DOT COLON
%token SUM MULT AVERAGE SHOW SHOWALL MAX SWITCH
%token EOF

  /*
%start singlecomm
%type <Command.comm> singlecomm
    */
      
%start debut
%type <Command.comm list> debut

%%
debut:
  | clist EOF { $1 }
;

clist:
  | singlecomm clist { $1::$2 }
  | singlecomm                { [$1] }
;

singlecomm:
  | cell EQUAL formula { Upd($1,$3) }
  | SHOW cell { Show($2) }
  | SHOWALL { ShowAll }
  | SWITCH sheet { SwitchTo($2) }
;

sheet:
  | SHEET INT { $2 }
;

cell:
  | CELLROW INT { ($1,$2) }
;

operand:
  | SUM { S }
  | MULT { M }
  | AVERAGE { A }
  | MAX { Max }
;

formula:
  | NBR { Cst (F $1) }
  | INT { Cst (I $1) }
  | cell { Cell (Cell.cellname_to_coord $1) }
  | operand LPAREN forlist RPAREN { Op($1,$3) }
  | sheet LPAREN cell SEMICOL cell RPAREN {Fnc($1 - 1, cellname_to_coord $3, cellname_to_coord $5)}
;

forlist:
  | formula { [$1] }
  | formula SEMICOL forlist { $1::$3 }
  | cell COLON cell { (Cell.interval_to_list (Cell.cellname_to_coord $1) (Cell.cellname_to_coord $3) ) }
  | cell COLON cell SEMICOL forlist { (Cell.interval_to_list (Cell.cellname_to_coord $1) (Cell.cellname_to_coord $3) )@$5 }
;
