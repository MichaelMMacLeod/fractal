#lang racket/base

(require "painter.rkt" racket/contract/base)

(provide (contract-out [painter painter?]))

(define (painter info)
  (define iterations (hash-ref info 'iterations))
  (define red (hash-ref info 'red))
  (define blue (hash-ref info 'blue))
  (define green (hash-ref info 'green))
  (argb-color 255
              (modulo (+ red iterations) 255)
              (modulo (+ blue iterations) 255)
              (modulo (+ green iterations) 255)))
