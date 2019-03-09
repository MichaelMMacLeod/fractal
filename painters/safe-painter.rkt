#lang racket/base

(require racket/contract/base)

(provide 
  (contract-out 
    (struct argb-color
      ([a exact-nonnegative-integer?]
       [r exact-nonnegative-integer?]
       [g exact-nonnegative-integer?]
       [b exact-nonnegative-integer?]))
    [painter? contract?]))

(struct argb-color (a r g b))

(define painter? (-> (hash/c symbol? any/c) argb-color?))
