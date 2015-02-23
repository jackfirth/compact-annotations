#lang scribble/manual

@(require scribble/eval
          (for-label compact-annotations
                     racket/base))

@(define compact-annotations-eval (make-base-eval #:lang 'typed/racket))
@(compact-annotations-eval '(require "main.rkt"))
@(define-syntax-rule (compact-annotations-examples datum ...)
   (examples #:eval compact-annotations-eval datum ...))

@title{Compact Annotations}

@defmodule[compact-annotations]

This library provides a more convenient syntax for writing typed racket
polymorphic and curried functions. It is very similar to Haskell type
annotations.

@author[@author+email["Jack Firth" "jackhfirth@gmail.com"]]

source code: @url["https://github.com/jackfirth/compact-annotations"]

@defform[
  #:literals (=> -> + *)
  (:: id maybe-polymorphic function-type)
  #:grammar ([maybe-polymorphic (code:line)
                                (code:line type-var-id ... =>)]
             [function-type (code:line arg-type ... maybe-opt maybe-rest -> function-type)
                            (code:line arg-type ... maybe-opt maybe rest -> result-type)]
             [maybe-opt (code:line)
                        (code:line + arg-type ...)]
             [maybe-rest (code:line)
                         (code:line * arg-type)])]{
  Declares that @racket[id] has the type determined by @racket[function-type],
  which may be a parametric type over @racket[type-var-id ...] if provided.
  The syntax of @racket[function-type] allows for partially applied function
  types, optional argument types, and rest argument types.
  
  With this syntax, the type of the identity function can be defined as follows:
  @compact-annotations-examples[
    (:: ident A => A -> A)
    (define (ident a) a)
    (:print-type ident)
    (ident 10)
  ]
  
  A function that takes one value and returns a function that takes another value
  and ignores it, returning the first value can have it's type defined as follows:
  @compact-annotations-examples[
    (:: first-value A B => A -> B -> A)
    (define ((first-value a) b) a)
    (:print-type first-value)
    ((first-value 'foo) 20)
  ]
  
  Optional arguments can have their type specified with a @code{+}. For example,
  a function that greets people with "Hello" by default can have it's type defined
  as follows:
  @compact-annotations-examples[
    (:: greet String + String -> String)
    (define (greet name [greeting "Hello"])
      (string-append greeting " " name))
    (:print-type greet)
    (greet "John")
    (greet "Jack" "Hey")
  ]
  
  Rest arguments can also have their type specified with a @code{*}. A function
  that converts it's arguments from one type to String then appends them all
  can have its type specified as follows:
  @compact-annotations-examples[
    (:: append-as-string A => (A -> String) * A -> String)
    (define (append-as-string a->string . as)
      (apply string-append (map a->string as)))
    (:print-type append-as-string)
    (append-as-string number->string 1 2 3 4 5)
  ]
}