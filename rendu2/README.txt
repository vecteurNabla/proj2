
pour compiler, lancer
make

pour executer le programme, lancer
./main.native

entrer une expression arithmetique (avec +, * et -)


il s'agit d'une calculette enrichie, ou l'on illustre un mecanisme
permettant de gerer le "-" unaire :

la calculette sait gerer des expressions comme 3*-2, ou 3--1 (mais pas
2++2). 

observez (dans parser.mly) qu'il y a deux regles parlant du token
MINUS : la regle pour le moins binaire, et la regle pour le moins
unaire. cette derniere est declaree comme la plus prioritaire par
l'intermediaire d'un mecanisme "ad hoc et sale" : on introduit un faux
token, UMINUS, qui a la priorite maximale (car il apparait apres TIMES
dans la liste des associativites), et auquel il est fait reference
avec "%prec" dans la regle de grammaire correspondante.


