# FouineJN - rendu 4 - mai 2021

Robin Jourde & Nicolas Nardino

```
                                                    `=xvCv!':^\czzc|;,          
                                                   .0vsFfU6Qe!,.``'~;LE8a6deyk, 
                                                   =yDfz\,r|:         :'?yRCoDR 
                                                   -0Q'                   `raQk 
                                                    wj                        O|
    @@@@   @@@   @   @  @@@  @   @  @@@@            Q=   'e$E:        oO0z   ~0!
    @     @   @  @   @   @   @@  @  @               ?g'  |8p8B,     `CQaQ|  ;#' 
    @@@   @   @  @   @   @   @ @ @  @@@              ,@c`  ``:w! ` `{:    '=g,  
    @     @   @  @   @   @   @  @@  @                 @;pu^`   zge8B-  'tu@;`   
    @      @@@    @@@   @@@  @   @  @@@@ (JN)         Q\l!:C9c- ^?+.`^e/` Q     
                                                      dai=   ,vp9yo6i~    p"    
                                                     .Q:;e     :z :m      lz    
                                                    `gs `#'     %-'D      :g    
                                                   '#z   'm     :E B      ,0    
         'iAg##N6/.    -\e%##8Xc,                 !B!     .a`    z|%"     vf    
       ,8@@@@@@@@@@A``X@@@@@@@@@@#:              iQ,       "O`   :#A;    .Q,    
      ;@@@@@@@@@@@@@##@@@@@@@@@@@@@+            |Q,         j7   -@d!   `R=     
      B@@@@@@@@@@@@@@@@@@@@@@@@@@@@g           '@:          fi   -@@`  `0v      
      Q@@@@@@@@@@@@@@@@@@@@@@@@@@@@Q          `#7           9+   ^@{   ue       
      3@@@@@@@@@@@@@@@@@@@@@@@@@@@@e         `Uk            %:   dm   ,Q`       
      `g@@@@@@@@@@@@@@@@@@@@@@@@@@N`        ~W\     '       #~  Cd    kv        
        u@@@@@@@@@@@@@@@@@@@@@@@@j         vd'      e      `Q. \Q!   \@,        
         -j@@@@@@@@@@@@@@@@@@@@j-         -de        ~a`    L% |:g  `9lK/       
            |D@@@@@@@@@@@@@@d=          `#$          \l    c9:7,D  w| `O\       
              .7Q@@@@@@@@Bi-            OD`          :d    =Q8`!6 '@`  `d|      
                 :m@@@@9:              lQ`            Q,   .@C =j '@d'  `B-     
                   -6e.               ,@!             |K    fj f\ '@+g;  mF     
                                      pm              `Q!    cEB' rN ze-!|B     
                                     ,@'              |ka+~,;{CU `B/ +Kya/6     
                                     t6               B,d>Lw!QDa xd   !dp|`     
                                     ma          '!=LFQ'!LOdu#=w Q:             
                                     o$         `:,''.:/uoC=`,DO Q:             
                                     |Q                   -+t3@@,R;             
                                     -@,                      'd\ei             
                                      $y                       `Qxe             
                                      :@,                       Rc%             
                                       %j                       Q%k             
                                       0C                      'QWl             
                                `'~;\uKx` ^R                   avQ,             
           `,^luuo3jffttttttfuuCzL=!,`    \@g!                ?dz6              
      -!vCCx=:`                          ,B|!@'             'w@OgKKWRpu+``  ``  
  '?jjv!`                            `:\9W\$9!           `!CB#kx7l=;ifkBv:,!'~  
\Ku:    '"``-'~,..':+\>>?/zywpp9k9kXmaL!` `Ql",,,,,,'.`      -!L\=jQD//+:,`:''` 
!jo3juujzlvzzzzzvc\?;!!::,'-`               ~=||=>>?||\lzzzzoe$gQ8e>`     `     
```

## Utilisation

Pour compiler, lancer `make`

Pour exécuter, lancer

`./fouine [-options] fichier.ml`

Pour connaître la liste des options, faire `./fouine --help`

Pour lancer les tests, faire `make test [OPT="-options"]`. Cela
exéctera `./fouine OPT fichier.ml` pour chaque fichier test
présent. Par défaut les options sont `-cps -autotest`.

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


### OCaml & traduction du `let rec`

La traduction du `let rec` m'a posé qqs soucis. La solution proposée
fonctionne mais pour être acceptée par OCaml, il faut appliquer les
bêta-réductions. En effet sinon on se retrouve avec des choses de la forme :

```ocaml
let rec f = ... (fun k -> k f) ...
```
Les bêta-réductions permettent de faire disparaitre ces formes.

## Bêta-réduction

Les options `-reduc` et `-optim` permettent de faire subir au code des
bêta-réductions pour le condenser (cf. `--help` pour plus de détails
sur ces options).

La fonction de réduction, située dans `reduction.ml`, détecte les
situations de la forme :

```ocaml
(fun x -> e) e'
```

où `e'` est un expression "irréductible" ie est une valeur, un
pattern, une fonction, un uple ou une liste d'irréductibles.

Elle parcourt ensuite `e` pour y remplacer toutes les occurences
*libres* de `x` par `e'`.

Les deux opérations sont en fait réalisées en même temps, pour ce
faire on maintient à jour lors du parcours une liste de couples
`(pattern à remplacer, expression irréductible)` que l'on utilise pour
savoir si et par quoi remplacer les pattern rencontrés.

Pour garantir que l'on ne remplace que les occurences libres, on
retire de la liste les pattern rencontrés dans des lieurs (`fun`,
`let`, `match`).

### Typage et CPS

Le typage par défaut a lieu *avant* la traduction CPS si elle doit avoir lieu. Si on souhaite en plus typer le code traduit, il faut utiliser l'option `-cpstypes`.

## Répartition du travail

### Nicolas

- passage de `eval` en cps

- implémentation de l'autotest


- monomorphisme

### Robin

- traduction et options associées

- réduction


- unification (à partir du TP10 th-prog)

- polymorphisme

## Tests

Les tests sont répartis en 5 dossiers : debutant, intermediaire,
avance, exceptions, transformation (les mêmes qu'au rendu précédent) et types.

On notera que `make test` ne renvoie pas que des `OK`, cela est dû aux spécificités de FouineJN par rapport à OCaml. Voir les descriptions ci-dessous pour plus d'informations.

### debutant

- `all.ml` toutes les fonctionnalités demandées aux débutants : les
  `let .. in`, les opérations arithmétiques, le `prInt` et le `if
  .. then .. else`

- `bool.ml` booléens : variables booléennes et opérateurs

- `cmp.ml` comparaisons entre entiers (constantes et variables)

### intermediaire

- `fact.ml` factorielle : défiinition de fonction récursive,
  application

- `fun_rec.ml` fonction récursive avec `fun .. ->`

- `fun_pls_arg.ml` fonction à plusieurs arguments (avec `let` et
  `fun`), fonction booléenne

- `fun_fun.ml` fonction avec fonction en argument (fonction
  d'évaluation), `not` est une fonction /!\ le non typage est exploité
  dans cet exemple : `eval` est polymorphe

- `app_partiel.ml` application partielle

### avance

*Remarque* : on utilise en fait depuis le début la séquence à travers
les différents tests

- `fact_n_imp.ml` factorielle avec compteur (référence d'entier)

- `ref_ref.ml` bidouille avec des reférences de fonctions polymorphes
  (/!\ n'est possible que grâce au non typage) : on vérifie que
  l'ordre est respecté : de droite à gauche pour les séquences et de
  gauche à droite pour les arguments. Parenthésage avec `begin
  .. end`. `prInt`, `ref` et `not` sont des fonctions qui pevent être
  passées en paramètre et stockées dans le tas. réfénernces de
  booléens et de fonctions.

- `tuples_args2.ml` couples : matching dans un let, priorité de la
  virgule

- `tuples_args3.ml` tuples : décurryfication (on peut déclarer une
  fonction décurryfiée sans mettre de parenthèses autour de ses
  arguments, ce qui est impossible en OCaml), tuples "partiels" /!\
  invalide en OCaml, cf remarque ci-desssous.

*Remarque* : En `FouineJN` les tuples sont des couples de couples, la
virgule étant associative à gauche. cela permet d'écrire des choses
qui ne sont pas autorisées en OCaml et d'utiliser des "tuples
partiels". Par exemple on peut matcher un triplet d'entiers avec un
couple ; ce dernier sera composé d'un entier et d'un couple d'entiers.

- `tuples_match` `match .. with` avec des tuples. Au passage on peut
  remarque que les paramètres de l'addition sont bien évalués de
  droite à gauche.

- `list_n_function.ml` calcule la longueur d'une liste en utilisant
  `function`

- `let_n_let.ml` différents `let` matchings avec des listes. Au
  passage on remarque que `FouineJN` ne permet pas encore d'accorder
  la priorité aux opératuers sur le `::`, les parenthèses autour du
  `-1` sont nécéssaires

- `functions.ml` deux `function` imbriqués

- `match.ml` deux match imbriqués

- `peano.ml` un exemple d'utilisation des listes et des macthing pour
  représenter les entiers de peano

- `dblsemi.ml` double semicolon (avec match et let .. in imbriqué)

### exceptions

- `raise_simple.ml` exception simple, non rattrapée : le dernière
  ligne n'est jamais exécutée

- `raise1.ml` excpetion avec matching d'une liste au rattrapage

*Remarque* : L'autotest renverra **NO** si des raise ne sont pas
rattrapés: en effet, notre syntaxe n'est pas identique à celle
d'OCaml pour les exceptions non rattrapées.

### transformation

- `collision_k_ident.ml` on utilise un identifiant qui sera utilisé
  lors de la transformation pour représenter une continuation, cela
  engendre une erreur de typage

### types

- `let.ml` déclaration simple

- `let2.ml` déclaration simple de fonction polymorphe

- `let3.ml` déclaration complexe de fonction polymorphe

- `let4.ml` identité appliquée à l'indentité

- `let5.ml` déclarations imbriquées, `if` et couples

- `let6.ml` `fun`, `=` et `fst`

- `let7.ml` `fun` et `let` imbriqués

- `let8.ml` double identité

- `let9.ml` déclarations d'une même variable plusieurs fois

- `rec.ml` fonction récursive simple

- `rec2.ml` polymorphe, longueur d'une liste qcq

- `rec3.ml` fonction qui s'appelle elle même : la fonction est typable mais son exéction ne termine pas. n'est pas excutée dans ce code

- `ref.ml` ref de fonctions

- `ref2.ml` ref ref

- `ref3.ml` ref de listes non typable

- `ref4.ml` une fonction avec variable globale

*Attention* Ce programme n'est normalement pas typable mais notre manière de traiter le typage de `ref` le permet. Le programme peut s'excuter en `FouineJN`.

- `exc.ml` exceptions avec types non entiers, sans typage ça marche, avec ça plante

- `exc2.ml` exceptions bien typées (entiers)

- `stdlib.ml` `=` polymorphe

- `stdlib2.ml` `<>`, `=`, `if`

- `stdlib3.ml` `+`, `*`, `fst`, `snd`
