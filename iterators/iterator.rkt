#lang racket/base

(require racket/contract/base (for-syntax racket/base))

(provide iterator? iterator-builder? define-iterator)

(define iterator? any/c)

(define iterator-builder? any/c)

(define-syntax (define-iterator stx)
  (syntax-case stx ()
    [(_ (iterator-name real imaginary info-keys ...) body ...)
     #'(define (iterator-name info-hash)
         (define info-keys (hash-ref info-hash 'info-keys))
         ...
         (lambda (real imaginary)
           body ...))]))
