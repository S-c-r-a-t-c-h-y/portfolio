# Calcul formel en OCaml (WIP)

Ce projet est une libraire OCaml de calcul formel.

## Fonctionnalités

- expression arithmétique comportant les expressions :
  - des nombres
  - des variables
  - des constantes :
    - pi
    - e
  - -
  - \*
  - /
  - ^
  - les fonctions :
    - exponentielle
    - logarithme néperien
    - sinus
    - cosinus
    - tangente
    - arcsinus
    - arccosinus
    - arctangente
    - sinus hyperbolique
    - cosinus hyperbolique
    - tangente hyperbolique
- dérivation d'une expression par rapport à une variable
- simplification d'une expression avec des simplifications usuelles
- evalutation d'une expression
- fonction à partir d'une expression
- parsing depuis une expression sous la forme d'une string

TODO :
simplification des fractions plus avancée
calcul de limite ?
intégration ?
prouver terminaison fonctions simplify et equal
cas d'égalité de fraction + revoir cas égalité Pow
ajouter nombres complexes
