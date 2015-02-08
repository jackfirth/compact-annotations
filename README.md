compact-annotations
===================

A Typed Racket package for writing compact polymorphic (and non-polymorphic) function annotations with a syntax similar to that of Haskell's type annotations.

Examples:

    (require compact-annotations)

    (:: flip A B => A -> (A -> B) -> B)
    (define ((flip v) f)
      (f v))
    
    (:: zip-with A B C => (A B -> C) -> (Listof A) (Listof B) -> (Listof C))
    (define ((zip-with zipper) as bs)
      (map zipper as bs))
      
    (:: compare-as A B => (A A -> Boolean) -> (B -> A) -> B B -> Boolean)
    (define (((compare-as base-compare) convert) v1 v2)
      (base-compare (convert v1) (convert v2)))

    (:: string-shorter? String String -> Boolean)
    (define string-shorter?
      (((inst compare-as Real) <) string-length))

    (:: in-range? Real Real + Boolean -> Real -> Boolean)
    (define ((in-range? low high [include-endpoints? #t]) x)
      ((if include-endpoints? <= <) low x high))
    
    (:: and? A => * (A -> Boolean) -> A -> Boolean)
    (define ((and? . ps) a)
      (andmap (λ ([p : (A -> Boolean)]) (p a)) ps))

To install, run `raco pkg install compact-annotations`.
