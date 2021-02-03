# Projet : rendu 1 - Tableur

Auteurs : Nicolas Nardino, Robin Jourde

Date : 3 février 2021

## Commentaires généraux
- La présente archive contient les fichiers de codes, un `make`, un dossier `test` contenant des fichiers de test et le présent readme.

- Tout le sujet a été traité jusqu'à la 3.4 (feuilles de calculs comme des
  fonctions) incluse, et fonctionne pour les tests effectués.

## Utilisation
- L'exécutable a deux options : `-paf`, l'option demandée par
  l'énoncé, et `-debug`, qui active le mode debug (i.e. met à `true`
  la variable définie dans `debug.ml`).

- L'exécutable lit sur l'entrée standard (comme demandé dans
  l'énoncé), ou depuis un fichier.

## Commentaires sur les tests
1. Premier exemple
2. Opérateur SUM et Show/ShowAll
3. Plusieurs opérateurs
4. Minimal
5. Modification
6. Dépendances entre cellules
7. idem
8. idem
9. Utilisation des intervalles
10. Dépendances plus complexes
11. Dépendances et intervalles
12. Autres dépendances
13. Montre que les différentes feuilles fonctionnent correctement
14. Montre le base des fonctions, et que les cellules qui dépendent
    d'une A1 d'une feuille sont bien recalculées lorsqu'on la fonction
    correspondante est évaluée
15. Montre qu'on peut composer des fonctions
16. Le crash n'est pas un bug, c'est le comportement attendu lorsque
    on définit une fonction récursive sans condition de terminaison
17. Un petit peu de tout...
