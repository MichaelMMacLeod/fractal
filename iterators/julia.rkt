#lang assembly-line

(require racket/require
         (for-syntax racket)
         (filtered-in
          (lambda (name)
            (and (regexp-match? #rx"^unsafe-fl" name)
                 (regexp-replace #rx"^unsafe-" name "")))
          racket/unsafe/ops))

(define-worker
  (julia [a flonum?]
         [bi flonum?]
         [max-iterations exact-nonnegative-integer?]
         [c-real flonum?]
         [c-imaginary flonum?])
  exact-nonnegative-integer?
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
