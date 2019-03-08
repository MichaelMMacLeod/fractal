#lang racket/base

(require racket/contract/base (for-syntax racket/base))

(provide iterator? define-iterator)

(define iterator? any/c)

(define-syntax (define-iterator stx)
  (syntax-case stx ()
    [(_ (iterator-name [info-keys info-contracts] ...) body ...)
     #'(begin
         (require racket/contract/base)

         (provide
          (contract-out [required-info-keys (listof (cons/c symbol? contract?))]
                        [rename iterator-name iterator iterator?]))

         (define required-info-keys
           (list (cons 'info-keys info-contracts) ...))

         (define (iterator-name info-hash)
           (define info-keys (hash-ref info-hash 'info-keys)) ...
           body ...))]))
