#lang typed/racket

(require "haskell-type-arrow.rkt")

(:: f A B => (A -> B) (Listof A) -> (Listof B))
(define (f mf as)
  (map mf as))

(:: $ A B => A -> (A -> B) -> B)
(define (($ a) f)
  (f a))

(:: compare-project A B => (B B -> Boolean) -> (A -> B) -> A A -> Boolean)
(define (((compare-project cmp) a->b) v1 v2)
  (cmp (a->b v1)
       (a->b v2)))

(:: compare-as-real A => (A -> Real) -> A A -> Boolean)
(define compare-as-real ((inst compare-project Real) <))

(:: string-shorter? String String -> Boolean)
(define string-shorter?
  (((inst compare-project Real) <) string-length))

(:: zipWith A B C => (A B -> C) -> (Listof A) (Listof B) -> (Listof C))
(define ((zipWith zipper) as bs)
  (map zipper as bs))

(:: map-list-to-vector A B => (Listof A) -> (A -> B) -> (Vectorof B))
(define ((map-list-to-vector lst) f)
  (list->vector (map f lst)))

(:: add-ints * Integer -> Integer)
(define add-ints +)

(:: in-range? Real Real + Boolean -> Real -> Boolean)
(define ((in-range? low high [exclusive? #f]) x)
  (if exclusive?
      (and (< x low) (> x high))
      (and (<= x low) (>= x high))))