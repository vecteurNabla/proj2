# FouineJN - rendu 1 - mars 2021
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


## tests

Les tests sont répartis en trois dossiers : debutant, intermediaire et avance.

### debutant

- `all.ml` toutes les fonctionnalités demandées aux débutants : les `let .. in`, les opérations arithmétiques, le `prInt` et le `if .. then .. else`

- `bool.ml` booléens : variables booléennes et opérateurs

- `cmp.ml` comparaisons entre entiers (constantes et variables)

### intermediaire

- `fact.ml` factorielle : défiinition de fonction récursive, application

- `fun_rec.ml` fonction récursive avec `fun .. ->`

- `fun_pls_arg.ml` fonction à plusieurs arguments (avec `let` et `fun`), fonction booléenne

- `fun_fun.ml` fonction avec fonction en argument (fonction d'évaluation), `not` est une fonction   /!\ le non typage est exploité dans cet exemple : `eval` est polymorphe

- `app_partiel.ml` application partielle

### avance

*Remarque* : on utilise en fait depuis le début la séquence à travers les différents tests

- `fact_n_imp.ml` factorielle avec compteur (référence d'entier)

- `ref_ref.ml` bidouille avec des reférences de fonctions polymorphes (/!\ n'est possible que grâce au non typage) : on vérifie que l'ordre est respecté : de droite à  gauche pour les séquences et de gauche à droite pour les arguments. Parenthésage avec `begin .. end`. `prInt`, `ref` et `not` sont des fonctions qui pevent être passées en paramètre et stockées dans le tas. réfénernces de booléens et de fonctions.

- `tuples_args2.ml` couples : matching dans un let, priorité de la virgule

- `tuples_args3.ml` tuples : décurryfication (on peut déclarer une fonction décurryfiée sans mettre de parenthèses autour de ses arguments, ce qui est impossible en OCaml), tuples "partiels" /!\ invalide en OCaml, cf remarque ci-desssous.

*Remarque* : En `FouineJN` les tuples sont des couples de couples, la virgule étant associative à gauche. cela permet d'écrire des choses qui ne sont pas autorisées en OCaml et d'utiliser des "tuples partiels". Par exemple on peut matcher un  triplet d'entiers avec un couple ; ce dernier sera composé d'un entier et d'un couple d'entiers.

- `tuples_match` `match .. with` avec des tuples. Au passage on peut remarque que les paramètres de l'addition sont bien évalués de droite à gauche.

- `list_n_function.ml` calcule la longueur d'une liste en utilisant `function`

- `let_n_let.ml` différents `let` matchings avec des listes. Au passage on remarque que `FouineJN` ne permet pas encore d'accorder la priorité aux opératuers sur le `::`, les parenthèses autour du `-1` sont nécéssaires

- `functions.ml` deux `function` imbriqués

- `match.ml` deux match imbriqués

- `peano.ml` un exemple d'utilisation des listes et des macthing pour représenter les entiers de peano
