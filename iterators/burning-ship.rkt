#lang racket/base

(require "iterator.rkt"
         (rename-in racket/unsafe/ops
                    [unsafe-fl+ fl+]
                    [unsafe-fl- fl-]
                    [unsafe-fl* fl*]
                    [unsafe-flsqrt flsqrt]
                    [unsafe-fl>= fl>=]
                    [unsafe-flabs flabs]))

(define-iterator (burning-ship [a flonum?]
                               [bi flonum?]
                               [max-iterations exact-nonnegative-integer?])
  (let loop ([z-real a]
             [z-imaginary bi]
             [iterations 0])
    (define z-real-square (fl* z-real z-real))
    (define z-imaginary-square (fl* z-imaginary z-imaginary))
    (cond [(or (fl>= (fl+ z-real-square z-imaginary) 4.0)
               (>= iterations max-iterations))
           iterations]
          [else (loop (flabs (fl+ (fl- z-real-square z-imaginary-square)
                                  a))
                      (fl+ (flabs (fl* 2.0 z-real z-imaginary))
                           bi)
                      (add1 iterations))])))
