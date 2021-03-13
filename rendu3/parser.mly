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
%token RAISE TRY E

/* precedences & associativities, form lowest to highest */
%nonassoc IN
%nonassoc LET FUN MATCH FUNCTION
%right SEQ
%nonassoc IF
%right AFF
%left PIPE
%left COMA
%right MAPS
%right OR
%right AND
%left EQUAL GT LT GEQ LEQ NEQ
%right CONS
%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS
%left APP E 					/* E is a constructor */



%start main             /* "start" signale le point d'entrée: */
                        /* c'est ici main, qui est défini plus bas */
%type <Expr.expr> main     /* on _doit_ donner le type associé au point d'entrée */

%%
/* Les listes récursives à gauche */

reverse_separated_nonempty_llist(separator, X):
  x = X
    { [ x ] }
| xs = reverse_separated_nonempty_llist(separator, X); separator; x = X
    { x :: xs }

%inline separated_nonempty_llist(separator, X):
   xs = reverse_separated_nonempty_llist(separator, X)  { List.rev xs }

reversed_preceded_or_separated_nonempty_llist(delimiter, X):
  ioption(delimiter) x = X
    { [x] }
| xs = reversed_preceded_or_separated_nonempty_llist(delimiter, X)
  delimiter
  x = X
    { x :: xs }

%inline preceded_or_separated_nonempty_llist(delimiter, X):
  xs = rev(reversed_preceded_or_separated_nonempty_llist(delimiter, X))
    { xs }

/* --- début des règles de grammaire --- */
/* à droite, les valeurs associées */

main:                       /* <- le point d'entrée (cf. + haut, "start") */
surface EOF      { $1 }  /* on veut reconnaître une expression */
;


surface:
    e = expression { e }
  | p = separated_nonempty_llist(DBLSEMICOL, let_decs); DBLSEMICOL; e = expression
	{ List.fold_left (fun x l ->
					   let l' = fst l in
					   if snd l then
						 Rec(fst l', snd l', x)
					   else
						 Let(fst l', snd l', x)) e p }
;


expression:			    /* règles de grammaire pour les expressions */
  | simpl_expr                                              { $1 }
  | expression SEQ expression                               { Let(Under, $1, $3) }
  | l = let_decs IN e = expression
	{ let l' = fst l in
	  if snd l then
	    Rec(fst l', snd l', e)
	  else
	    Let(fst l', snd l', e) }
  | app_expr                                                { $1 }
  | FUN fun_expr                                            { $2 }
  | IF expression THEN expression ELSE expression %prec IF  { If($2, $4, $6) }
  | expression COMA expression                              { Cpl($1, $3) }
  | expression AFF expression                               { Aff($1, $3) }
  | RAISE LPAREN E e = expression RPAREN %prec APP                          { Raise e }
  | TRY; e = expression; WITH E; p = pattern; MAPS; e1 = expression
	{ Try(e, p, e1) }
  | MATCH expression WITH pattern_matching                  { Match($2, $4) }
  | FUNCTION pattern_matching
	{ Fun(Ident "@", Match(Pattern (Ident "@"), $2)) }
  | l = reverse_separated_nonempty_llist(CONS, atom_expr); CONS; a = atom_expr /* il faudrait remplacer atom_expr par simpl_expr */
	{ List.fold_left (fun x b -> Cons(b, x)) a l }
;

let_decs:
  | LET let_binding %prec LET                     { ($2, false) }
  | LET REC let_binding %prec LET                 { ($3, true)}
;

simpl_expr:
  | atom_expr                                               { $1 }
  | expr_infix                                              { $1 }
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
  | MINUS expression %prec UMINUS         { App(App(Pattern (Ident "(-)"), Const (Int 0)), $2) }
  | expression; o = op; expression        { App(App(Pattern (Ident o), $1), $3) }
;
%inline op:
  | PLUS  {"(+)"}
  |	TIMES {"(*)"}
  |	MINUS {"(-)"}
  |	DIV	  {"(/)"}
  |	AND	  {"(&&)"}
  |	OR	  {"(||)"}
  |	LEQ	  {"(<=)"}
  |	GEQ	  {"(>=)"}
  |	LT	  {"(<)"}
  |	GT	  {"(>)"}
  |	EQUAL {"(=)"}
  |	NEQ	  {"(<>)"}
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
  | l = reverse_separated_nonempty_llist(SEQ, atom_expr)
	{ List.fold_left (fun x b -> Cons(b, x)) (Const Nil) l }
;

/* list_pt: */
/*   | expression CONS atom_expr      { Cons ($1, $3) } */
/*   | expression CONS list_pt        { Cons ($1, $3) } */

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
  | pattern                           { PList_cons($1, PConst Nil) }
;

pattern_matching:
  | l = preceded_or_separated_nonempty_llist(PIPE, match_case)
	{ l }
;
match_case:
  | s = separated_pair(pattern, MAPS, expression) %prec PIPE { s }
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
