#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [the-width (parameter/c (or/c #f exact-nonnegative-integer?))]
  [the-height (parameter/c (or/c #f exact-nonnegative-integer?))]
  [the-iterator-path (parameter/c (or/c #f module-path?))]
  [the-painter-path (parameter/c (or/c #f module-path?))]
  [the-worker-count (parameter/c (or/c #f exact-positive-integer?))]
  [the-center-real (parameter/c (or/c #f flonum?))]
  [the-center-imaginary (parameter/c (or/c #f flonum?))]
  [the-zoom (parameter/c (or/c #f flonum?))]
  [the-zoom-factor (parameter/c (or/c #f flonum?))]
  [the-default-cache-value (parameter/c (or/c #f byte?))]
  [the-info (parameter/c (or/c #f (hash/c symbol? any/c)))]))

(define the-width (make-parameter #f))
(define the-height (make-parameter #f))
(define the-iterator-path (make-parameter #f))
(define the-painter-path (make-parameter #f))
(define the-worker-count (make-parameter #f))
(define the-center-real (make-parameter #f))
(define the-center-imaginary (make-parameter #f))
(define the-zoom (make-parameter #f))
(define the-zoom-factor (make-parameter #f))
(define the-default-cache-value (make-parameter #f))
(define the-info (make-parameter #f))
