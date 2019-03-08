#lang racket/base

(require "iterator.rkt"
         racket/contract/base
         (rename-in racket/unsafe/ops
                    [unsafe-fl+ fl+]
                    [unsafe-fl- fl-]
                    [unsafe-fl* fl*]
                    [unsafe-flsqrt flsqrt]
                    [unsafe-fl>= fl>=]
                    [unsafe-flabs flabs])
         racket/match)

(provide
 (contract-out
  [rename burning-ship iterator iterator?]))

(define-iterator (burning-ship a bi max-iterations)
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
