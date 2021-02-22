%{
    (* --- préambule: ici du code Caml --- *)

    open Expr   (* rappel: dans expr.ml: 
                type expr = Const of int | Add of expr*expr | Mull of expr*expr *)

         %}
/* description des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */

%token <int> INT
%token <Expr.var> VAR
%token PLUS TIMES MINUS DIV
%token EQUAL GEQ LEQ GT LT
%token AND OR NOT TRUE FALSE
%token IF THEN ELSE
%token LET IN
%token FUN MAPS
%token LPAREN RPAREN
%token EOF


/* precedences & associativities, form lowest to highest */
%nonassoc LET FUN
%nonassoc IF
%right OR
%right AND
%left EQUAL GT LT GEQ LEQ
%left PLUS MINUS 
%left TIMES DIV
%nonassoc UMINUS NOT 			/* NOT will have to go when implemented as a fun */



%start main             /* "start" signale le point d'entrée: */
/* c'est ici main, qui est défini plus bas */
%type <Expr.expr> main     /* on _doit_ donner le type associé au point d'entrée */

%%
/* --- début des règles de grammaire --- */
/* à droite, les valeurs associées */


main:                       /* <- le point d'entrée (cf. + haut, "start") */
expression EOF                { $1 }  /* on veut reconnaître une expression */
;


expression:			    /* règles de grammaire pour les expressions */
  | constant                                                { $1 }
  | LPAREN expression RPAREN                                { $2 }
  | expression PLUS expression                              { Add($1,$3) }
  | expression TIMES expression                             { Mul($1,$3) }
  | expression MINUS expression                             { Min($1,$3) }
  | MINUS expression %prec UMINUS                           { Min(Const 0, $2) }
  | VAR                                                     { Var $1 }
  | LET let_binding IN expression %prec LET                 { Let(fst $2, snd $2, $4) }
  /* | LET REC let_binding IN expression %prec LET             { Rec(fst $2, snd $2, $4)} */
  /* il faudra revoir le type REC pour ça */
  | FUN VAR MAPS expression %prec FUN                       { Fun($2, $4) }
  | IF expression THEN expression ELSE expression %prec IF  { If($2, $4, $6) }
  | expression AND expression                               { And($1 ,$3) }
  | expression OR expression                                { Or($1 ,$3) }
  | expression LEQ expression                               { Leq($1, $3) }
  | expression GEQ expression                               { Geq($1, $3) }
  | expression LT expression                                { Lt($1, $3) }
  | expression GT expression                                { Gt($1, $3) }
  | NOT expression                                          { Not $2 } /* also has to go */
;

constant:
  | INT                           { Const $1 }
  | TRUE                          { True }
  | FALSE                         { False }
;

let_binding:
  | VAR EQUAL expression   { ($1, $3) }
  | VAR let_binding        { ($1, Fun(fst $2, snd $2)) }
