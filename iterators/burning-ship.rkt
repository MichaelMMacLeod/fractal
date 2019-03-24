#lang racket/base

(require "iterator.rkt"
         racket/require
         (for-syntax racket/base)
         (filtered-in (lambda (name)
                        (regexp-replace #rx"unsafe-" name ""))
                      racket/unsafe/ops))

(define-iterator (burning-ship [a flonum?]
                               [bi flonum?]
                               [max-iterations fixnum?])
  (let loop ([z-real a]
             [z-imaginary bi]
             [iterations 0])
    (define z-real-square (fl* z-real z-real))
    (define z-imaginary-square (fl* z-imaginary z-imaginary))
    (cond [(or (fl>= (fl+ z-real-square z-imaginary) 4.0)
               (fx>= iterations max-iterations))
           iterations]
          [else (loop (flabs (fl+ (fl- z-real-square z-imaginary-square)
                                  a))
                      (fl+ (flabs (fl* 2.0 z-real z-imaginary))
                           bi)
                      (fx+ 1 iterations))])))
