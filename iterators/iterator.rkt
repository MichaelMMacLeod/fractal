#lang racket/base

(require racket/contract/base)

(provide 
 (contract-out [iterator? contract?] 
               [iterator-builder? contract?]))

(define iterator?
  (-> flonum? flonum? exact-nonnegative-integer?))

(define iterator-builder?
  (-> (hash/c symbol? any/c) iterator?))
