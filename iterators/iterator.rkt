#lang racket/base

(require racket/contract/base (for-syntax racket/base syntax/to-string))

(provide iterator? define-iterator)

(define iterator? any/c)

(define-syntax (define-iterator stx)
  (syntax-case stx ()
    [(_ (iterator-name [keys contracts] ...) body ...)
     #`(begin
         (require racket/contract/base)

         (provide
          (contract-out [info-contracts (hash/c symbol? contract?)]
                        [info-contract-strings (hash/c symbol? string?)]
                        [iterator iterator?]))

         (define info-contracts
           (make-hash (list (cons 'keys contracts) ...)))

         (define info-contract-strings
           (make-hash
            (list #,@(for/list ([key (in-list (syntax->list #'(keys ...)))]
                                [key-contract (in-list (syntax->list #'(contracts ...)))])
                       #`(cons 'key #,(syntax->string #`(#,key-contract)))))))

         (define (iterator info-hash)
           (define (iterator-name)
             (define keys (hash-ref info-hash 'keys)) ...
             body ...)
           (hash-set info-hash 'iterations (iterator-name))))]))
