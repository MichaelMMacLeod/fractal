#lang racket/base

(require racket/contract/base)

(provide 
 (contract-out [iterator? contract?]))

(define iterator?
  (-> (hash/c symbol? any/c) exact-nonnegative-integer?))
