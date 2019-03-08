#lang racket/base

(require racket/contract/base
         (for-syntax racket/base syntax/to-string))

(provide (struct-out argb-color)
         painter?
         define-painter)

(struct argb-color (a r g b) #:prefab)

(define painter? any/c)

(define-syntax (define-painter stx)
  (syntax-case stx ()
    [(_ (painter-name [keys contracts] ...) body ...)
     #`(begin
         (require racket/contract/base)

         (provide
          (contract-out [info-contracts (hash/c symbol? contract?)]
                        [info-contract-strings (hash/c symbol? string?)]
                        [painter painter?]))

         (define info-contracts
           (make-hash (list (cons 'keys contracts) ...)))

         (define info-contract-strings
           (make-hash
            (list
             #,@(for/list ([key (in-list (syntax->list #'(keys ...)))]
                           [key-contract (in-list (syntax->list #'(contracts ...)))])
                  #`(cons 'key #,(syntax->string #`(#,key-contract)))))))

         (define (painter info-hash)
           (define (painter-name)
             (define keys (hash-ref info-hash 'keys)) ...
             body ...)
           (hash-set info-hash 'color (painter-name))))]))
