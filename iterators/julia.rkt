#lang racket/base

(require "iterator.rkt"
         (rename-in racket/unsafe/ops
                    [unsafe-fl+ fl+]
                    [unsafe-fl- fl-]
                    [unsafe-fl* fl*]
                    [unsafe-flsqrt flsqrt]
                    [unsafe-fl>= fl>=]))

(define-iterator (julia [a flonum?]
                        [bi flonum?]
                        [c-real flonum?]
                        [c-imaginary flonum?]
                        [max-iterations exact-nonnegative-integer?])
  (let loop ([z-real a]
             [z-imaginary bi]
             [iterations 0])
    (define z-real-square (fl* z-real z-real))
    (define z-imaginary-square (fl* z-imaginary z-imaginary))
    (cond [(or (fl>= (fl+ z-real-square z-imaginary-square) 4.0)
               (>= iterations max-iterations))
           iterations]
          [else (loop (fl+ (fl- z-real-square
                                z-imaginary-square)
                           c-real)
                      (fl+ (fl* 2.0
                                (fl* z-real z-imaginary))
                           c-imaginary)
                      (add1 iterations))])))
