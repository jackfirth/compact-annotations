#lang scribble/manual

@(require scribble/eval
          (for-label compact-annotations
                     racket/base))

@(define compact-annotations-eval (make-base-eval))
@(compact-annotations-eval '(require "main.rkt"))
@(define-syntax-rule (compact-annotations-examples datum ...)
   (examples #:eval compact-annotations-eval datum ...))

@title{Compact Annotations}

@defmodule[compact-annotations]


@author[@author+email["Jack Firth" "jackhfirth@gmail.com"]]

source code: @url["https://github.com/jackfirth/compact-annotations"]
