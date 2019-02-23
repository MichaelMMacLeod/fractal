#lang racket/base

(require "painter.rkt" racket/contract/base)

(provide (contract-out [build-painter painter-builder?]))

(define ((build-painter info) iterations)
  (define v (cond [(> iterations 255)
                   (- 255 (modulo iterations 255))]
                  [else iterations]))
  (argb-color 255 v v v))
