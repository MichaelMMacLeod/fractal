#lang racket/base

(require "iterators/iterator.rkt"
         "painters/painter.rkt"
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
  (bytes-set! bytestring pos (argb-color-a color))
  (bytes-set! bytestring (+ pos 1) (argb-color-r color))
  (bytes-set! bytestring (+ pos 2) (argb-color-g color))
  (bytes-set! bytestring (+ pos 3) (argb-color-b color)))

(define (render-part!
         bytestring
         start-index end-index
         center-real center-imaginary
         width height
         zoom
         iterator
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
    (define iterator-info (hash-set* info 'a a 'bi bi))
    (define iterations (iterator iterator-info))
    (define painter-info (hash-set* iterator-info 'iterations iterations))
    (define color (painter painter-info))
    (insert-argb-color! bytestring index color)))
