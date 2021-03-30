# FouineJN - rendu 3 - aril 2021

## Utilisation

Pour compiler, lancer
`make`

Pour exécuter, lancer

`./fouine [-options] fichier.ml`

Pour connaître la liste des options, faire `./fouine --help`

Pour lancer les tests, faire `make test [OPT="-options"]`. Cela exéctera `./fouine OPT fichier.ml` pour chaque fichier test présent. Par défaut les options sont `-cps -autotest`.

## Remarques

Il reste un shift/reduce conflict que l'on ne parvient pas à corriger,
dans ce genre de cas:

```
match x with
| a -> e1
| b -> match y with
	    | c -> e3
		| d -> e4   (* ici *)
```

Menhir ne sait pas si la ligne `| d -> e4` doit rentrer dans le
premier filtrage ou le deuxième. Il résoud "arbitrairement" en faveur
du shift (i.e. la fait rentrer dans le deuxième match), qui est ce
qu'OCaml fait; ainsi tout se comporte comme on peut s'y attendre

Le dossier "version_restreinte" comporte une demi-solution:
restreindre ce qui peut arriver à droite d'une flèche dans un cas de
filtrage (dans notre grammaire, `atom_expr` au lieu de
`expression`). Cela revient à devoir mettre des parenthèses autour du
membre droit d'un cas de filtrage dans la plupart des cas. On peut donc
exprimer les mêmes choses, mais avec plus de parenthèses qu'en OCaml.

## Répartition du travail

**Nicolas** :

- passage de `eval` en cps

- implémentation de l'autotest

**Robin** :

- traduction et options associées

## Tests

Les tests sont répartis en trois dossiers : debutant, intermediaire, avance (les mêmes qu'au rendu précédent), exceptions et transformation

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

- `dblsemi.ml` double semicolon (avec match et let .. in imbriqué)

### exceptions

- `raise_simple.ml` exception simple, non rattrapée : le dernière ligne n'est jamais exécutée

- `raise1.ml` excpetion avec matching d'une liste au rattrapage

### transformation

- `collision_k_ident.ml` échoue sans transfomation mais fonctionne avec : on utilise un identifiant qui sera utilisé lors de la transformation pour représenter une continuation
