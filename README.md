compact-annotations
===================

A Typed Racket package for writing compact polymorphic (and non-polymorphic) function annotations with a syntax similar to that of Haskell's type annotations.

Examples:

    (require compact-annotations)

    (:: flip A B => A -> (B -> A) -> B)
    (define ((flip v) f)
      (f v))
    
    (:: zipWith A B C => (A B -> C) -> (Listof A) (Listof B) -> (Listof C))
    (define ((zipWith zipper) as bs)
      (map zipper as bs))
      
    (:: compareAs A B => (A A -> Boolean) -> (B -> A) -> B B -> Boolean)
    (define (((compareAs base-compare) convert) v1 v2)
      (base-compare (convert v1) (convert v2)))
