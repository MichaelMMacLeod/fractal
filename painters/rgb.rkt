#lang assembly-line

(require "painter.rkt")

(define-worker (rgb [iterations exact-nonnegative-integer?]
                    [red exact-nonnegative-integer?]
                    [green exact-nonnegative-integer?]
                    [blue exact-nonnegative-integer?])
  argb-color?
  (argb-color 255
              (modulo (+ red iterations) 255)
              (modulo (+ blue iterations) 255)
              (modulo (+ green iterations) 255)))
