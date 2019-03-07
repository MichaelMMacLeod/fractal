#lang racket/base

(require "iterators/iterator.rkt"
         "painters/painter.rkt"
         "render.rkt"
         racket/contract/base
         racket/flonum
         racket/match
         racket/place
         racket/serialize
         assembly-line)

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
     [info any/c]))
  [create-workers
   (-> module-path? module-path? exact-positive-integer? (listof place?))]))

(struct worker-message
  (bytestring
   start-index end-index
   center-real center-imaginary
   width height
   zoom
   info)
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
           info)
          (define deserialized-info (deserialize info))
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
                   (build-iterator deserialized-info)
                   (build-painter deserialized-info)))))])))))
