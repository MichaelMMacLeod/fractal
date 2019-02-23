#lang racket/base

(require racket/contract/base)

(provide iterator? iterator-builder?)

(define iterator? any/c)

(define iterator-builder? any/c)
