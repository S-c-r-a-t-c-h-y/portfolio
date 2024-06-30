# Calcul formel en OCaml (WIP)

Ce projet est une libraire OCaml de calcul formel.

## Fonctionnalités

- expression arithmétique comportant les expressions :
  - des nombres
  - des variables numérotées par des entiers
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
    - Arcsinus
    - Arccosinus
    - Arctangente
- dérivation d'une expression par rapport à une variable
- simplification d'une expression avec des simplifications usuelles
- evalutation d'une expression
- fonction à partir d'une expression

TODO :
simplification des fractions plus avancée
parsing depuis une expression réelle
calcul de limite ?
intégration ?
prouver terminaison fonctions simplify et equal
cas d'égalité de fraction + revoir cas égalité Pow
