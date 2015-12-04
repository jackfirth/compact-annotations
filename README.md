compact-annotations [![Build Status](https://travis-ci.org/jackfirth/compact-annotations.svg)](https://travis-ci.org/jackfirth/compact-annotations) [![Coverage Status](https://coveralls.io/repos/jackfirth/compact-annotations/badge.svg)](https://coveralls.io/r/jackfirth/compact-annotations)
===================

[![Join the chat at https://gitter.im/jackfirth/compact-annotations](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/jackfirth/compact-annotations?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[Documentation](http://pkg-build.racket-lang.org/doc/compact-annotations/index.html)

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
