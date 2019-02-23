#lang racket/base

(require "workers.rkt"
         racket/class
         racket/contract/base
         racket/flonum
         racket/match
         racket/place
         racket/serialize
         (only-in racket/draw bitmap%))

(provide
 (contract-out
  (struct state
    ([center-real flonum?]
     [center-imaginary flonum?]
     [zoom flonum?]
     [width exact-nonnegative-integer?]
     [height exact-nonnegative-integer?]
     [cache bytes?]
     [cache-length exact-nonnegative-integer?]
     [bitmap (is-a?/c bitmap%)]
     [workers (listof place?)]
     [iterator-info (hash/c symbol? any/c)]
     [painter-info (hash/c symbol? any/c)])
    #:omit-constructor)
  [make-state
   (->* (exact-nonnegative-integer?
         exact-nonnegative-integer?
         module-path?
         module-path?
         (hash/c symbol? any/c)
         (hash/c symbol? any/c))
        (#:center-real flonum?
         #:center-imaginary flonum?
         #:zoom flonum?
         #:worker-count exact-positive-integer?)
        state?)]
  [move-center/state
   (-> state?
       exact-nonnegative-integer?
       exact-nonnegative-integer?
       state?)]
  [generate-new-cache/state (-> state? state?)]
  [redraw-cache!/state (-> state? state?)]
  [zoom/state (-> state? flonum? state?)]
  [generate-new-bitmap/state (-> state? state?)]
  [redraw-bitmap!/state (-> state? state?)]
  [resize/state
   (-> state?
       exact-nonnegative-integer?
       exact-nonnegative-integer?
       state?)]))

(struct state
  (center-real
   center-imaginary
   zoom
   width
   height
   cache
   cache-length
   bitmap
   workers
   iterator-info
   painter-info)
  #:constructor-name -state)

(define (make-state
         width
         height
         iterator-path
         painter-path
         iterator-info
         painter-info
         #:center-real [center-real 0.0]
         #:center-imaginary [center-imaginary 0.0]
         #:zoom [zoom 0.005]
         #:worker-count [worker-count (processor-count)])
  (define cache-length (* 4 width height))
  (-state
   center-real
   center-imaginary
   zoom
   width
   height
   (make-shared-bytes cache-length 50)
   cache-length
   (make-object bitmap% width height)
   (create-workers iterator-path painter-path worker-count)
   iterator-info
   painter-info))

(define (move-center/state s screen-x screen-y)
  (match-define
    (struct* state ([width width]
                    [height height]
                    [center-real center-real]
                    [center-imaginary center-imaginary]
                    [zoom zoom]))
    s)
  (define new-center-real
    (fl+ center-real
         (fl* zoom
              (fl- (->fl screen-x)
                   (fl/ (->fl width)
                        2.0)))))
  (define new-center-imaginary
    (fl+ center-imaginary
         (fl* zoom
              (fl- (->fl screen-y)
                   (fl/ (->fl height)
                        2.0)))))
  (struct-copy state s
              [center-real new-center-real]
              [center-imaginary new-center-imaginary]))

(define (generate-new-cache/state s)
  (match-define
    (struct* state ([width width] [height height]))
    s)
  (define new-cache-length (* 4 width height))
  (define new-cache (make-shared-bytes new-cache-length 50))
  (struct-copy state s
               [cache new-cache]
               [cache-length new-cache-length]))

(define (redraw-cache!/state s)
  (match-define
    (struct* state
             ([width width]
              [height height]
              [center-real center-real]
              [center-imaginary center-imaginary]
              [zoom zoom]
              [workers workers]
              [cache cache]
              [cache-length cache-length]
              [iterator-info iterator-info]
              [painter-info painter-info]))
    s)
  (define work-length (quotient cache-length (length workers)))
  (for ([worker (in-list workers)]
        [start-index (in-range 0 cache-length work-length)])
    (define message
      (worker-message
       cache
       start-index
       (+ start-index work-length)
       center-real
       center-imaginary
       width
       height
       zoom
       ;; We use serialize here to avoid a sigsev relating to prefab structs with contracts
       ;; and hash maps. For more information, see these issues:
       ;;  - https://github.com/racket/racket/issues/2504
       ;;  - (possibly related) https://github.com/racket/racket/issues/2298
       (serialize iterator-info)
       (serialize painter-info)))
    (place-channel-put worker message))
  s)

(define (zoom/state s zoom-factor)
  (struct-copy state s
               [zoom (* (state-zoom s) zoom-factor)]))

(define (generate-new-bitmap/state s)
  (match-define
    (struct* state ([bitmap bitmap]
                    [width width]
                    [height height]
                    [cache cache]))
    s)
  (define new-bitmap (make-object bitmap% width height))
  (send new-bitmap set-argb-pixels 0 0 width height cache)
  (struct-copy state s [bitmap new-bitmap]))

(define (redraw-bitmap!/state s)
  (match-define
    (struct* state ([bitmap bitmap]
                    [width width]
                    [height height]
                    [cache cache]))
    s)
  (send bitmap set-argb-pixels 0 0 width height cache)
  s)

(define (resize/state s new-width new-height)
  (generate-new-bitmap/state
   (generate-new-cache/state
    (struct-copy state s [width new-width] [height new-height]))))
