#lang racket/base

(require "mandelbrot.rkt"
         racket/class
         racket/flonum
         racket/future
         racket/place
         (only-in racket/draw bitmap%)
         (for-syntax racket/base racket/syntax))

(provide (all-defined-out))

(define-syntax (introduce-fields stx)
  (syntax-case stx ()
    [(_ (struct-id struct-expr) fields ...)
     (with-syntax ([(accessors ...)
                    (for/list ([field (in-list (syntax->list #'(fields ...)))])
                      (format-id #'field "~a-~a"
                                 (syntax-e #'struct-id)
                                 (syntax-e field)))])
       #'(begin (define fields (accessors struct-expr)) ...))]))

(struct state
  (center-real
   center-imaginary
   zoom
   max-iterations
   width
   height
   cache
   cache-length
   bitmap
   workers)
  #:transparent
  #:constructor-name -state)

(define (make-state
         width
         height
         #:center-real [center-real 0.0]
         #:center-imaginary [center-imaginary 0.0]
         #:zoom [zoom 0.005]
         #:max-iterations [max-iterations 500]
         #:worker-count [worker-count (processor-count)])
  (-state
   center-real
   center-imaginary
   zoom
   max-iterations
   width
   height
   (make-shared-bytes (* 4 width height) 50)
   (* 4 width height)
   (make-object bitmap% width height)
   (create-workers worker-count)))

(define (update-state
         s
         #:center-real [center-real #f]
         #:center-imaginary [center-imaginary #f]
         #:zoom [zoom #f]
         #:max-iterations [max-iterations #f]
         #:width [width #f]
         #:height [height #f]
         #:cache [cache #f]
         #:cache-length [cache-length #f]
         #:bitmap [bitmap #f]
         #:workers [workers #f])
  (-state
   (if center-real center-real (state-center-real s))
   (if center-imaginary center-imaginary (state-center-imaginary s))
   (if zoom zoom (state-zoom s))
   (if max-iterations max-iterations (state-max-iterations s))
   (if width width (state-width s))
   (if height height (state-height s))
   (if cache cache (state-cache s))
   (if cache-length cache-length (state-cache-length s))
   (if bitmap bitmap (state-bitmap s))
   (if workers workers (state-workers s))))

(define (move-center/s state screen-x screen-y)
  (introduce-fields
   (state state)
   center-real center-imaginary zoom width height)

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

  (update-state
   state
   #:center-real new-center-real
   #:center-imaginary new-center-imaginary))

(define (generate-new-cache/s state)
  (introduce-fields
   (state state)
   width height)

  (define new-cache-length (* 4 width height))
  (define new-cache (make-shared-bytes new-cache-length 50))

  (update-state
   state
   #:cache new-cache
   #:cache-length new-cache-length))

(define (redraw-cache!/s state)
  (introduce-fields
   (state state)
   width height center-real center-imaginary zoom max-iterations workers
   cache cache-length)

  (define work-length (quotient cache-length (length workers)))

  (for ([worker (in-list workers)]
        [worker-id (in-naturals)]
        [start-index (in-range 0 cache-length work-length)])
    (place-channel-put
     worker
     (worker-message
      worker-id
      cache
      start-index
      (+ start-index work-length)
      center-real
      center-imaginary
      width
      height
      zoom
      max-iterations)))

  state)

(define (zoom/s state zoom-factor)
  (update-state
   state
   #:zoom (* (state-zoom state) zoom-factor)))

(define (generate-new-bitmap/s state)
  (introduce-fields
   (state state)
   width height cache)

  (define new-bitmap (make-object bitmap% width height))

  (send new-bitmap set-argb-pixels 0 0 width height cache)

  (update-state state #:bitmap new-bitmap))

(define (redraw-bitmap!/s state)
  (introduce-fields
   (state state)
   bitmap width height cache)

  (send bitmap set-argb-pixels 0 0 width height cache)

  state)

(define (resize/s state new-width new-height)
  (generate-new-bitmap/s
   (generate-new-cache/s
    (update-state state #:width new-width #:height new-height))))