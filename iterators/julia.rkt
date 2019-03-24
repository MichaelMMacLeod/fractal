#lang racket/base

(require "iterator.rkt"
         racket/require
         (for-syntax racket/base)
         (filtered-in (lambda (name)
                        (regexp-replace #rx"unsafe-" name ""))
                      racket/unsafe/ops))

(define-iterator (julia [a flonum?]
                        [bi flonum?]
                        [c-real flonum?]
                        [c-imaginary flonum?]
                        [max-iterations fixnum?])
  (let loop ([z-real a]
             [z-imaginary bi]
             [iterations 0])
    (define z-real-square (fl* z-real z-real))
    (define z-imaginary-square (fl* z-imaginary z-imaginary))
    (cond [(or (fl>= (fl+ z-real-square z-imaginary-square) 4.0)
               (fx>= iterations max-iterations))
           iterations]
          [else (loop (fl+ (fl- z-real-square
                                z-imaginary-square)
                           c-real)
                      (fl+ (fl* 2.0
                                (fl* z-real z-imaginary))
                           c-imaginary)
                      (fx+ 1 iterations))])))
