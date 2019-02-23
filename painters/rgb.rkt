#lang racket/base

(require "painter.rkt" racket/contract/base racket/match)

(provide (contract-out [build-painter painter-builder?]))

(define/match (build-painter info)
  [((hash-table ('red red) ('green green) ('blue blue)))
    (lambda (iterations)
      (argb-color 255 
                  (modulo (+ red iterations) 255)
                  (modulo (+ blue iterations) 255)
                  (modulo (+ green iterations) 255)))])
