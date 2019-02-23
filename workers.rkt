#lang racket/base

(require "iterators/iterator.rkt"
         "painters/painter.rkt"
         racket/contract/base
         racket/flonum
         racket/match
         racket/place
         racket/serialize)

(provide
 (contract-out
  (struct worker-message
    ([bytestring bytes?]
     [start-index exact-nonnegative-integer?]
     [end-index exact-nonnegative-integer?]
     [center-real flonum?]
     [center-imaginary flonum?]
     [width exact-nonnegative-integer?]
     [height exact-nonnegative-integer?]
     [zoom flonum?]
     ;[iterator-info (hash/c symbol? any/c)]
     ;[painter-info (hash/c symbol? any/c)]
     [iterator-info any/c]
     [painter-info any/c]
     ))
  [create-workers
   (-> module-path? module-path? exact-positive-integer? (listof place?))]))

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
         iterate
         paint)
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
    (define color (paint (iterate a bi)))
    (insert-argb-color! bytestring index color)))

(struct worker-message
  (bytestring
   start-index end-index
   center-real center-imaginary
   width height
   zoom
   iterator-info
   painter-info)
  #:prefab)

(define (create-workers iterator-path painter-path count)
  (for/list ([worker-number (in-range count)])
    (place/context
     channel
     (define build-iterator (dynamic-require iterator-path 'build-iterator))
     (define build-painter (dynamic-require painter-path 'build-painter))
     (let loop ([worker-thread #f])
       (match (place-channel-get channel)
         [(worker-message
           bytestring
           start-index end-index
           center-real center-imaginary
           width height
           zoom
           iterator-info
           painter-info)
          (cond [worker-thread (kill-thread worker-thread)]
                [else (void)])
          (loop (thread
                 (lambda ()
                  (render-part!
                   bytestring
                   start-index end-index
                   center-real center-imaginary
                   width height
                   zoom
                   (build-iterator (deserialize iterator-info))
                   (build-painter (deserialize painter-info))))))])))))
