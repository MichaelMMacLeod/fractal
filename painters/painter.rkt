#lang racket/base

(require racket/contract/base)

(provide (struct-out argb-color) painter?)

(struct argb-color (a r g b) #:prefab)

(define painter? any/c)
