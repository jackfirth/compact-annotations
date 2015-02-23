#lang typed/racket

(require (for-syntax syntax/parse
                     racket/syntax
                     racket/list))

(provide ::)

(define-for-syntax (map-syntax f stx)
  (map f (syntax->list stx)))

(define-for-syntax ((bound-in? bound? id) stx)
  (if (identifier? stx)
      (bound? id stx)
      (ormap (bound-in? bound? id)
             (or (syntax->list stx) '()))))

(define-for-syntax ((type-var-in-args arg-type-stxs) type-var-id)
  ((bound-in? bound-identifier=? type-var-id) arg-type-stxs))

(define-for-syntax (All:: type-vars-stx result-type-stx)
  (syntax-parse type-vars-stx
    [(type-var:id ...)
     (syntax-parse result-type-stx
       [(-> arg-type ...+ result-type)
        (let-values ([(bound-type-vars free-type-vars)
                      (partition (type-var-in-args #'(arg-type ...))
                                 (syntax->list #'(type-var ...)))])
          (with-syntax* ([(free-type-var ...) free-type-vars]
                         [(bound-type-var ...) bound-type-vars]
                         [result-type (if (empty? free-type-vars)
                                          #'result-type
                                          (All:: #'(free-type-var ...) #'result-type))])
            (if (empty? bound-type-vars)
                #'(-> arg-type ... result-type)
                #'(All (bound-type-var ...) (-> arg-type ... result-type)))))])]))

(define-for-syntax func::
  (syntax-parser #:literals (->)
    [((~and (~not ->) expr-arg) ... (~literal ->) expr-result ...)
     (func-contract:: #'(expr-arg ...) (func:: #'(expr-result ...)))]
    [(other) #'other]
    [other #'other]))

(define-for-syntax (func-contract:: arg-stx result-stx)
  (syntax-parse arg-stx #:literals (+ *)
    [((~and (~not +) (~not *) expr-arg) ... * (~and (~not *) (~not +) rest-arg))
     #`(->* (#,@(map-syntax func:: #'(expr-arg ...)))
            ()
            #:rest #,(func:: #'rest-arg)
            #,result-stx)]
    [((~and (~not +) (~not *) expr-arg) ... + (~and (~not *) (~not +) opt-arg) ...+)
     #`(->* (#,@(map-syntax func:: #'(expr-arg ...)))
            (#,@(map-syntax func:: #'(opt-arg ...)))
            #,result-stx)]
    [((~and (~not +) (~not *) expr-arg) ...)
     #`(-> #,@(map-syntax func:: #'(expr-arg ...))
           #,result-stx)]))

(define-syntax (:: stx)
  (syntax-parse stx #:literals (=>)
    [(_ binding:id type-var:id ...+ => ~! contract-expr ...+)
     #`(: binding #,(All:: #'(type-var ...) (func:: #'(contract-expr ...))))]
    [(_ binding:id contract-expr ...)
     #`(: binding #,(func:: #'(contract-expr ...)))]))