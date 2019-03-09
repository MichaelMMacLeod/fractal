#lang racket/base

(require "iterators/iterator.rkt"
         "painters/painter.rkt"
         "render.rkt"
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
     [info any/c]
     [iterator-path module-path?]
     [painter-path module-path?]))
  [create-workers
   (-> module-path? module-path? exact-positive-integer? (listof place?))]))

(struct worker-message
  (bytestring
   start-index end-index
   center-real center-imaginary
   width height
   zoom
   info
   iterator-path
   painter-path)
  #:prefab)

(define (create-workers iterator-path painter-path count)
  (for/list ([worker-number (in-range count)])
    (place/context
     channel

     (define iterator (dynamic-require iterator-path 'iterator))

     (define painter (dynamic-require painter-path 'painter))

     (let loop ([worker-thread #f])
       (match (place-channel-get channel)
         [(worker-message
           bytestring
           start-index end-index
           center-real center-imaginary
           width height
           zoom
           info
           new-iterator-path
           new-painter-path)

          (define deserialized-info (deserialize info))

          (cond [worker-thread (kill-thread worker-thread)]
                [else (void)])

          (cond [(equal? new-iterator-path iterator-path) (void)]
                [else
                 (set! iterator-path new-iterator-path)
                 (set! iterator (dynamic-require iterator-path 'iterator))])

          (cond [(equal? new-painter-path painter-path) (void)]
                [else
                 (set! painter-path new-painter-path)
                 (set! painter (dynamic-require painter-path 'painter))])

          (loop (thread
                 (lambda ()
                  (render-part!
                   bytestring
                   start-index end-index
                   center-real center-imaginary
                   width height
                   zoom
                   iterator
                   painter
                   deserialized-info))))])))))
