#lang racket/gui

(require "mandelbrot.rkt" racket/flonum (for-syntax racket/syntax))

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
   cache-needs-update
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
   #t
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
         #:cache-needs-update [cache-needs-update #f]
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
   (if cache-needs-update cache-needs-update (state-cache-needs-update s))
   (if bitmap bitmap (state-bitmap s))
   (if workers workers (state-workers s))))

(define mandelbrot-canvas%
  (class canvas%
    (super-new)

    (define (make-initial-state)
      (make-state 600 600))

    (define state (make-initial-state))

    (define (update-center s canvas-x canvas-y)
      (introduce-fields (state s)
        center-real center-imaginary zoom width height)

      (define new-center-real
        (fl+ center-real
             (fl* zoom
                  (fl- (->fl canvas-x)
                       (fl/ (->fl width)
                            2.0)))))
      (define new-center-imaginary
        (fl+ center-imaginary
             (fl* zoom
                  (fl- (->fl canvas-y)
                       (fl/ (->fl height)
                            2.0)))))
      (update-state
       s
       #:center-real new-center-real
       #:center-imaginary new-center-imaginary))

    (define (generate-new-cache!)
      (introduce-fields (state state)
        width height)

      (define new-cache-length (* 4 width height))
      (define new-cache (make-shared-bytes new-cache-length 50))
      (define new-state
        (update-state
         state
         #:cache new-cache
         #:cache-length new-cache-length
         #:cache-needs-update #t))

      (set! state new-state))

    (define (update-cache s)
      (introduce-fields (state s)
       width height center-real center-imaginary zoom max-iterations workers cache cache-length)

      (define work-length (quotient cache-length
                                    (length workers)))

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

      (define new-state
        (update-state
         s
         #:cache-needs-update #f))

      new-state)

    (define (zoom s factor)
      (update-state
       s
       #:zoom (* (state-zoom s) factor)
       #:cache-needs-update #t))

    (define (update-bitmap s)
      (introduce-fields (state s)
        width height cache)

      (define new-bitmap (make-object bitmap% width height))

      (send new-bitmap set-argb-pixels 0 0 width height cache)

      (update-state s #:bitmap new-bitmap))

    (define/public (recache-bitmap!)
      (introduce-fields (state state)
        bitmap width height cache)
      (send bitmap set-argb-pixels 0 0 width height cache))

    (define (draw-cache s)
      (introduce-fields (state s)
        width height cache-length cache)

      (define bitmap (state-bitmap s))
      (define dc (send this get-dc))

      (cond [(= (* 4 width height) cache-length)
             (send bitmap set-argb-pixels 0 0 width height cache)]
            [else (void)])

      (send dc draw-bitmap bitmap 0 0))

    (define/override (on-event event)
      (cond [(send event button-down? 'left)
             (define new-state
               (update-cache
                (update-state
                 (update-center
                  state
                  (send event get-x)
                  (send event get-y))
                #:cache-needs-update #t)))
             (set! state new-state)
             (send this refresh)]
            [else (void)]))

    (define/override (on-char event)
      (cond [(eq? #\i (send event get-key-code))
             (define new-state
               (zoom state 0.9))
             (set! state new-state)
             (send this refresh)]
            [(eq? #\o (send event get-key-code))
             (define new-state
               (zoom state 1.1))
             (set! state new-state)
             (send this refresh)]))

    (define/override (on-paint)
      (cond [(state-cache-needs-update state)
             (define new-state
               (update-bitmap
                (update-cache
                 state)))
             (set! state new-state)]
            [else (draw-cache state)]))

    (define/override (on-size new-width new-height)
      (define new-state
        (update-cache
         (update-state
          state
          #:width new-width
          #:height new-height
          #:cache-needs-update #t)))
      (set! state new-state)
      (generate-new-cache!))

     (thread (lambda ()
              (for ([forever (in-naturals)])
                (draw-cache state)
                (sleep 0.05)
                )))

    ))
