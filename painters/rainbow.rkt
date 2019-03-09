#lang racket

(require "painter.rkt" colors racket/unsafe/ops)

(define number-of-colors 50)

(define colors
  (for/vector #:length number-of-colors
      ([h (in-range 0 1 (/ 1 number-of-colors))])
    (define c (hsl->color (hsl h 0.5 0.5)))
    (argb-color (inexact->exact (send c alpha))
                (send c red)
                (send c green)
                (send c blue))))

(define black (argb-color 1 0 0 0))

(define-painter (rainbow [iterations exact-nonnegative-integer?]
                         [max-iterations exact-nonnegative-integer?])
  (cond [(= iterations max-iterations) black]
        [else (unsafe-vector-ref colors (modulo iterations number-of-colors))]))
