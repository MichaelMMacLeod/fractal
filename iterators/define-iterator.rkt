#lang racket/base

(require (for-syntax "iterator.rkt"
                     racket/base
                     syntax/parse
                     syntax/strip-context))

(provide define-iterator)

(define-for-syntax (compile-iterator/typed stx)
  (syntax-parse stx
    [(_ (name:id argument:typed-variable ...) body:toplevel-form ... return:form)
     #`(module
           #,@(syntax-local-introduce
               (strip-context
                #'(iterator:typed typed/racket
                                  (provide iterator)
                                  (struct color ([r : Float]
                                                 [g : Float]
                                                 [b : Float]
                                                 [a : Float])
                                    #:prefab)
                                  (define (iterator argument ...) : color
                                    body ...
                                    return)))))]))


#;(define-for-syntax (compile-iterator/opengl stx)
  ''stx)

(define-syntax (define-iterator stx)
  (syntax-parse stx
    [i:iterator
     #`(begin
         #,(compile-iterator/typed stx)
         #;#,(compile-iterator/opengl stx))]))
