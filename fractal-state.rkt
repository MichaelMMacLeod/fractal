
#lang racket/base

(require "workers.rkt"
         racket/class
         racket/contract/base
         racket/draw
         racket/flonum
         racket/place
         racket/serialize)

(provide
 (contract-out
  [fractal-state%
   (class/c
    (init-field [width exact-nonnegative-integer?]
                [height exact-nonnegative-integer?]
                [iterator-path module-path?]
                [painter-path module-path?]
                [info (hash/c symbol? any/c)]
                [worker-count exact-positive-integer?]
                [center-real flonum?]
                [center-imaginary flonum?]
                [zoom flonum?]
                [zoom-factor flonum?]
                [default-cache-value byte?])
    [get-bitmap (->m (is-a?/c bitmap%))]
    [move-center (->m exact-nonnegative-integer?
                      exact-nonnegative-integer?
                      void?)]
    [generate-new-cache (->m void?)]
    [redraw-cache (->m void?)]
    [zoom-in (->m void?)]
    [zoom-out (->m void?)]
    [generate-new-bitmap (->m void?)]
    [redraw-bitmap (->m void?)]
    [resize (->m exact-nonnegative-integer?
                 exact-nonnegative-integer?
                 void?)]
    [get-iterator-path (->m module-path?)]
    [set-iterator-path (->m module-path? void?)]
    [get-painter-path (->m module-path?)]
    [set-painter-path (->m module-path? void?)]
    [get-info (->m (hash/c symbol? any/c))]
    [set-info (->m (hash/c symbol? any/c) void?)]
    [get-worker-count (->m exact-positive-integer?)]
    [get-center-real (->m flonum?)]
    [get-center-imaginary (->m flonum?)]
    [get-zoom (->m flonum?)]
    [get-zoom-factor (->m flonum?)]
    [get-default-cache-value (->m byte?)]
    [set-default-cache-value (->m byte? void?)]
    [save-bitmap (->*m ((or/c path-string? output-port?)
                        (or/c 'png 'jpeg 'xbm 'xpm 'bmp))
                       ((integer-in 0 100)
                        #:unscaled? any/c)
                       boolean?)]
    )]))

(define fractal-state%
  (class object%
    (super-new)

    (init-field width
                height
                iterator-path
                painter-path
                info
                [worker-count (processor-count)]
                [center-real 0.0]
                [center-imaginary 0.0]
                [zoom 0.05]
                [zoom-factor 0.05]
                [default-cache-value 50])

    (define cache-length (* 4 width height))
    (define cache (make-shared-bytes cache-length default-cache-value))
    (define bitmap (make-object bitmap% width height))
    (define workers (create-workers iterator-path painter-path worker-count))

    (define/public (get-bitmap) bitmap)

    (define/public (get-iterator-path) iterator-path)

    (define/public (set-iterator-path new-iterator-path)
      (set! iterator-path new-iterator-path))

    (define/public (get-painter-path) painter-path)

    (define/public (set-painter-path new-painter-path)
      (set! painter-path new-painter-path))

    (define/public (get-info) info)

    (define/public (set-info new-info)
      (set! info new-info))

    (define/public (get-worker-count) worker-count)

    (define/public (get-center-real) center-real)

    (define/public (get-center-imaginary) center-imaginary)

    (define/public (get-zoom) zoom)

    (define/public (get-zoom-factor) zoom-factor)

    (define/public (get-default-cache-value) default-cache-value)

    (define/public (set-default-cache-value new-default-cache-value)
      (set! default-cache-value new-default-cache-value))

    (define/public (move-center screen-x screen-y)
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
      (set! center-real new-center-real)
      (set! center-imaginary new-center-imaginary))

    (define/public (generate-new-cache)
      (set! cache-length (* 4 width height))
      (set! cache (make-shared-bytes cache-length default-cache-value)))

    (define/public (redraw-cache)
      (let ([work-length (quotient cache-length worker-count)])
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
             (serialize info)
             iterator-path
             painter-path))
          (place-channel-put worker message))))

    (define/public (zoom-in)
      (set! zoom (fl* zoom (fl- 1.0 zoom-factor))))

    (define/public (zoom-out)
      (set! zoom (fl* zoom (fl+ 1.0 zoom-factor))))

    (define/public (generate-new-bitmap)
      (set! bitmap (make-object bitmap% width height))
      (redraw-bitmap))

    (define/public (redraw-bitmap)
      (send bitmap set-argb-pixels 0 0 width height cache))

    (define/public (resize new-width new-height)
      (set! width new-width)
      (set! height new-height)
      (generate-new-cache)
      (generate-new-bitmap))

    (define/public (save-bitmap name kind [quality 75] #:unscaled? [unscaled? #f])
      (send bitmap save-file name kind quality #:unscaled? unscaled?))

    ))
