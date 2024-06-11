# construction-Glushkov

## Spécifications

Le code fournit permet à la fois le traitement des expressions en notations **postfixe**, mais également en notation **infixe** à la manière d'une expression régulière écrite à la main.

Sont implémentées les fonctionnalités suivantes :

- alternative avec `|`
- étoile de Kleene avec `*`
- caractère optionnel avec `?`
- concaténation avec `@` en notation postifxe, et concaténation implicite en notation infixe
- `.` pour matcher n'importe quel caractère
- en notation infixe, l'usage de parenthèses dans les expressions régulières est supporté, par exemple `(a|b)*`

## Compilation

- Pour compiler le fichier permettant le traitement des expressions en notation infixe, exectuer `ocamlopt -o mygrep_infixe infixe.ml`
- Pour compiler le fichier permettant le traitement des expressions en notation postfixe, exectuer `ocamlopt -o mygrep_postfixe postfixe.ml`

## Utilisation

Pour executer le fichier en notation infixe, utiliser `./mygrep_infixe <expression_régulière> [source]`
Pour executer le fichier en notation postfixe, utiliser `./mygrep_postfixe <expression_régulière> [source]`

Où :

- `<expression_régulière>` est votre expression régulière dans les bonnes conventions entre guillemets
- `[source]` est l'éventuel fichier qui doit être lu par le programme, ne rien mettre pour lire l'entrée standard

Lors d'une lecture depuis l'entrée standard, la ligne est répétée si elle match l'expression régulière et rien ne se passe sinon.

## Remarque

Les codes pour les deux fichiers sont les mêmes à l'exception de la construction de l'expression régulière et de sa linéarisation.
