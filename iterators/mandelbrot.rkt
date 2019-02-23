#lang racket/base

(require "iterator.rkt" racket/contract/base racket/flonum racket/match)

(provide (contract-out [build-iterator iterator-builder?]))

(define/match (build-iterator info)
  [((hash-table ('max-iterations max-iterations)))
   (lambda (a bi)
     (let loop ([z-real 0.0]
                [z-imaginary 0.0]
                [iterations 0])
       (define z-real-square (fl* z-real z-real))
       (define z-imaginary-square (fl* z-imaginary z-imaginary))
       (cond [(or (fl>= (flsqrt (fl+ z-real-square
                                     z-imaginary-square))
                        2.0)
                  (>= iterations max-iterations))
              iterations]
             [else (loop (fl+ (fl- z-real-square
                                   z-imaginary-square)
                              a)
                         (fl+ (fl* 2.0
                                   (fl* z-real z-imaginary))
                              bi)
                         (add1 iterations))])))])
