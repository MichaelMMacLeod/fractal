#lang racket/base

(require racket/contract/base (for-syntax racket/base))

(provide iterator? define-iterator)

(define iterator? any/c)

(define-syntax (define-iterator stx)
  (syntax-case stx ()
    [(_ (iterator-name info-keys ...) body ...)
     #'(define (iterator-name info-hash)
         (define info-keys (hash-ref info-hash 'info-keys)) ...
         body ...)]))
