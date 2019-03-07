#lang assembly-line

(require "painter.rkt")

(define-worker (grayscale [iterations exact-nonnegative-integer?])
  argb-color?
  (define v (cond [(> iterations 255)
                   (- 255 (modulo iterations 255))]
                  [else iterations]))
  (argb-color 255 v v v))
