%{
    (* --- préambule: ici du code Caml --- *)

    open Expr

%}
/* description des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */

%token <int> INT
%token <Expr.pattern> VAR
%token UNDER
%token PLUS TIMES MINUS DIV
%token EQUAL GEQ LEQ GT LT NEQ
%token AND OR
%token <bool> BOOL
%token IF THEN ELSE
%token LET IN REC
%token FUN MAPS FUNCTION
%token UNIT AFF DER SEQ
%token LPAREN RPAREN
%token EOF
%token DBLSEMICOL
%token CONS LSQB RSQB NIL
%token COMA
%token MATCH WITH PIPE

/* precedences & associativities, form lowest to highest */
%nonassoc LET FUN MATCH FUNCTION
%right SEQ
%nonassoc IF
%right AFF
%left COMA
%right OR
%right AND
%left EQUAL GT LT GEQ LEQ NEQ
%right CONS
%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS
%left APP



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
  | atom_expr                                               { $1 }
  | expr_infix                                              { $1 }
  | LET let_binding IN expression %prec LET                 { Let(fst $2, snd $2, $4) }
  | LET REC let_binding IN expression %prec LET             { Rec(fst $3, snd $3, $5)}
  | FUN fun_expr                                            { $2 }
  | IF expression THEN expression ELSE expression %prec IF  { If($2, $4, $6) }
  | app_expr                                                { $1 }
  | expression SEQ expression                               { Let(Under, $1, $3) }
  | expression COMA expression                              { Cpl($1, $3) }
  | expression AFF expression                               { Aff($1, $3) }
  /* | MATCH expression WITH pattern_matching %prec MATCH      { Match($2, $4) } */
  /* | FUNCTION pattern_matching %prec FUNCTION                { Fun(Ident "@", Match(Pattern (Ident "@"), $2)) } */
  | list_pt %prec CONS                                      { $1 }
;

atom_expr:
  | constant                                                { Const $1 }
  | LPAREN expression RPAREN                                { $2 }
  | VAR                                                     { Pattern $1 }
  | UNDER                                                   { Pattern Under }
  | DER atom_expr                                           { Der $2 }
  | pars_list                                               { $1 }
;

expr_infix:
  | MINUS expression %prec UMINUS                 { App(App(Pattern (Ident "(-)"), Const (Int 0)), $2) }
  | expression PLUS expression                    { App(App(Pattern (Ident "(+)"), $1), $3) }
  | expression TIMES expression                   { App(App(Pattern (Ident "(*)"), $1), $3) }
  | expression MINUS expression                   { App(App(Pattern (Ident "(-)"), $1), $3) }
  | expression DIV expression                     { App(App(Pattern (Ident "(/)"), $1), $3) }
  | expression AND expression                     { App(App(Pattern (Ident "(&&)"), $1), $3) }
  | expression OR expression                      { App(App(Pattern (Ident "(||)"), $1), $3) }
  | expression LEQ expression                     { App(App(Pattern (Ident "(<=)"), $1), $3) }
  | expression GEQ expression                     { App(App(Pattern (Ident "(>=)"), $1), $3) }
  | expression LT expression                      { App(App(Pattern (Ident "(<)"), $1), $3) }
  | expression GT expression                      { App(App(Pattern (Ident "(>)"), $1), $3) }
  | expression EQUAL expression                   { App(App(Pattern (Ident "(=)"), $1), $3) }
  | expression NEQ expression                     { App(App(Pattern (Ident "(<>)"), $1), $3) }
;

constant:
  | INT                           { Int $1 }
  | BOOL                          { Bool $1 }
  | UNIT                          { Unit }
  | NIL                           { Nil }
;

pars_list:
  | LSQB list_sh RSQB             { $2 }
;

list_sh:						/* [x_1; ... x_n] */
  | atom_expr SEQ list_sh       { Cons($1, $3) }
  | atom_expr                   { Cons($1, Const Nil) }
;

list_pt:
  | expression CONS atom_expr      { Cons ($1, $3) }
  | expression CONS list_pt        { Cons ($1, $3) }

let_binding:
  | pattern EQUAL expression                  { ($1, $3) }
  | pattern let_binding               { ($1, Fun(fst $2, snd $2)) }
;

pattern:
  | LPAREN pattern RPAREN                      { $2 }
  | VAR                                        { $1 }
  | UNDER                                      { Under }
  | constant                                   { PConst $1 }
  | pattern COMA pattern                       { PCpl($1,$3) }
  | pattern CONS pattern                       { PList_cons($1, $3) }
  | LSQB pattern_list_sh RSQB                  { $2 }
;
pattern_list_sh:						/* [x_1; ... x_n] */
  | pattern SEQ pattern_list_sh       { PList_cons($1, $3) }
  | pattern                   { PList_cons($1, PConst Nil) }
;

pattern_matching:
  | PIPE? l = separated_nonempty_list(PIPE, p = match_case { p })
	{ l }
;
match_case:
  | s = separated_pair(pattern, MAPS, expression) %prec MATCH { s }
;

fun_expr:
  | VAR MAPS expression %prec FUN    { Fun($1, $3) }
  | VAR fun_expr                     { Fun($1, $2) }

app_expr:
  | atom_expr argument %prec APP      { App($1, $2) }
  | app_expr argument %prec APP       { App($1, $2) }
;

argument:
  | atom_expr { $1 }
;
