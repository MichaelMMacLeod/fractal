#lang racket/base

(require "iterators/iterator.rkt"
         "painters/painter.rkt"
         (submod "glsl/mandelbrot.rkt" iterator:racket)
         racket/contract/base
         racket/flonum
         racket/match
         racket/performance-hint
         racket/place)

(provide
 (contract-out
  [render-part!
   (-> bytes?
       exact-nonnegative-integer?
       exact-nonnegative-integer?
       flonum?
       flonum?
       exact-nonnegative-integer?
       exact-nonnegative-integer?
       flonum?
       iterator?
       painter?
       (hash/c symbol? any/c)
       void?)]))

(define-inline (row-major-index->point index width)
  (values (remainder index width) (quotient index width)))

(define-inline (screen-point->complex-point
                x y
                center-real center-imaginary
                width height
                zoom)
  (values (fl+ center-real
               (fl* zoom
                    (fl- (->fl x)
                         (fl/ (->fl width)
                              2.0))))
          (fl+ center-imaginary
               (fl* zoom
                    (fl- (->fl y)
                         (fl/ (->fl height)
                              2.0))))))


(define-inline (insert-argb-color! bytestring index color)
  (define pos (* index 4))
  (bytes-set! bytestring pos (color-alpha color))
  (bytes-set! bytestring (+ pos 1) (color-red color))
  (bytes-set! bytestring (+ pos 2) (color-green color))
  (bytes-set! bytestring (+ pos 3) (color-blue color)))

(define (render-part!
         bytestring
         start-index end-index
         center-real center-imaginary
         width height
         zoom
         old-iterator
         painter
         info)
  (for ([index (in-range (quotient start-index 4)
                         (quotient end-index 4))])
    (define-values (x y)
      (row-major-index->point index width))
    (define-values (a bi)
      (screen-point->complex-point
       x y
       center-real center-imaginary
       width height
       zoom))
    ;(define iterator-info (hash-set* info 'a a 'bi bi))
    ;(define color (hash-ref (painter (iterator iterator-info)) 'color))
    (define color (iterator a bi))
    (insert-argb-color! bytestring index color)))
