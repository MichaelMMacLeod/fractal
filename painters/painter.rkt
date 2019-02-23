#lang racket/base

(require racket/contract/base)

(provide (struct-out argb-color) painter?  painter-builder?)

(struct argb-color (a r g b))

(define painter? any/c)

(define painter-builder? any/c)
