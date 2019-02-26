#lang racket/base

(require "iterator.rkt" 
         racket/contract/base 
         (rename-in racket/unsafe/ops
                    [unsafe-fl+ fl+]
                    [unsafe-fl- fl-]
                    [unsafe-fl* fl*]
                    [unsafe-flsqrt flsqrt]
                    [unsafe-fl>= fl>=])
         racket/match)

(provide (contract-out [build-iterator iterator-builder?]))

(define/match (build-iterator info)
  [((hash-table ('c-real c-real)
                ('c-imaginary c-imaginary)
                ('max-iterations max-iterations)))
   (lambda (a bi)
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
                         (add1 iterations))])))])
