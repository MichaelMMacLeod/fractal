#lang racket/base

(require "painter.rkt" racket/contract/base)

(provide (contract-out [painter painter?]))

(define (painter info)
  (define iterations (hash-ref info 'iterations))
  (define v (cond [(> iterations 255)
                   (- 255 (modulo iterations 255))]
                  [else iterations]))
  (argb-color 255 v v v))
