#lang racket/base

(require "iterators/iterator.rkt"
         "painters/painter.rkt"
         racket/contract/base
         racket/flonum
         racket/match
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
       void?)]))

(define (row-major-index->point index width)
  (values (remainder index width) (quotient index width)))

(define (screen-point->complex-point
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

(define (insert-argb-color! bytestring index color)
  (bytes-set! bytestring index (argb-color-a color))
  (bytes-set! bytestring (+ index 1) (argb-color-r color))
  (bytes-set! bytestring (+ index 2) (argb-color-g color))
  (bytes-set! bytestring (+ index 3) (argb-color-b color)))

(define (render-part!
         bytestring
         start-index end-index
         center-real center-imaginary
         width height
         zoom
         iterator
         painter)
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
    (define color (painter (iterator a bi)))
    (insert-argb-color! bytestring index color)))
