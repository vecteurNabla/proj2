%{
(* --- pr�ambule: ici du code Caml --- *)

open Expr   (* rappel: dans expr.ml: 
             type expr = Const of int | Add of expr*expr | Mull of expr*expr *)

%}
/* description des lex�mes, ceux-ci sont d�crits (par vous) dans lexer.mll */

%token <int> INT       /* le lex�me INT a un attribut entier */
%token PLUS TIMES MINUS
%token LPAREN RPAREN
%token EOL             /* retour � la ligne */

%left PLUS MINUS  /* associativit� gauche: a+b+c, c'est (a+b)+c */
%left TIMES  /* associativit� gauche: a*b*c, c'est (a*b)*c */
%nonassoc UMINUS  /* un "faux token", correspondant au "-" unaire */
                  /* cf. son usage plus bas : il sert � "marquer" une r�gle pour lui donner la pr�c�dence maximale */

		    
%start main             /* "start" signale le point d'entr�e: */
                        /* c'est ici main, qui est d�fini plus bas */
%type <Expr.expr> main     /* on _doit_ donner le type associ� au point d'entr�e */

%%
    /* --- d�but des r�gles de grammaire --- */
                            /* � droite, les valeurs associ�es */


main:                       /* <- le point d'entr�e (cf. + haut, "start") */
expression EOL                { $1 }  /* on veut reconna�tre une expression */
  ;
  

  expression:			    /* r�gles de grammaire pour les expressions */
  | INT                                 { Const $1 }
  | LPAREN expression RPAREN            { $2 } /* on r�cup�re le deuxi�me �l�ment */
  | expression PLUS expression          { Add($1,$3) }
  | expression TIMES expression         { Mul($1,$3) }
  | expression MINUS expression         { Min($1,$3) }
  | MINUS expression %prec UMINUS       { Min(Const 0, $2) }
;

