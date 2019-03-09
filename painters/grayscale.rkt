#lang racket/base

(require "painter.rkt")

(define-painter (grayscale [iterations exact-nonnegative-integer?])
  (define v (cond [(> iterations 255)
                   (- 255 (modulo iterations 255))]
                  [else iterations]))
  (argb-color 255 v v v))
