#lang racket/gui

(require racket/flonum)

(define (escape-time center-real center-imaginary max-iterations)
  (let loop ([z-real 0.0]
             [z-imaginary 0.0]
             [iterations 0])
    (define z-real-square (fl* z-real z-real))
    (define z-imaginary-square (fl* z-imaginary z-imaginary))
    (cond [(or (fl>= (flsqrt (fl+ z-real-square
                                  z-imaginary-square))
                     2.0)
               (>= iterations max-iterations))
           iterations]
          [else (loop (fl+ (fl- z-real-square
                                z-imaginary-square)
                           center-real)
                      (fl+ (fl* 2.0
                                (fl* z-real z-imaginary))
                           center-imaginary)
                      (add1 iterations))])))

(struct argb-color (a r g b))

(define (get-color iterations)
  (define v (modulo (* iterations 25) 255))
  (argb-color 255 v v v))

(define (mandelbrot!
         bytestring
         bytestring-length
         center-real
         center-imaginary
         width
         height
         zoom
         max-iterations)
  (for ([i (in-range 0 bytestring-length)]
        [position (in-range 0 bytestring-length 4)])
    (define x (remainder i width))
    (define y (quotient i width))

    (define the-real-part
      (fl+ center-real
           (fl* zoom
                (fl- (->fl x)
                     (fl/ (->fl width)
                          2.0)))))

    (define the-imaginary-part
      (fl+ center-imaginary
           (fl* zoom
                (fl- (->fl y)
                     (fl/ (->fl height)
                          2.0)))))
    (define iterations
      (escape-time the-real-part
                   the-imaginary-part
                   max-iterations))

    (define color (get-color iterations))

    (bytes-set! bytestring position (argb-color-a color))
    (bytes-set! bytestring (+ position 1) (argb-color-r color))
    (bytes-set! bytestring (+ position 2) (argb-color-g color))
    (bytes-set! bytestring (+ position 3) (argb-color-b color))))

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
   bitmap)
  #:transparent
  #:constructor-name -state)

(define (make-state
         width
         height
         #:center-real [center-real 0.0]
         #:center-imaginary [center-imaginary 0.0]
         #:zoom [zoom 0.005]
         #:max-iterations [max-iterations 50])
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
   (make-object bitmap% width height)))

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
         #:place-channels [place-channels #f])
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
   (if bitmap bitmap (state-bitmap s))))

(define mandelbrot-canvas%
  (class canvas%
    (super-new)

    (define (make-initial-state)
      (make-state 600 600))

    (define state (make-initial-state))

    (define (update-center s canvas-x canvas-y)
      (define center-real (state-center-real s))
      (define center-imaginary (state-center-imaginary s))
      (define zoom (state-zoom s))
      (define width (state-width s))
      (define height (state-height s))
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

    (define (update-cache s)
      (define width (state-width s))
      (define height (state-height s))
      (define center-real (state-center-real s))
      (define center-imaginary (state-center-imaginary s))
      (define zoom (state-zoom s))
      (define max-iterations (state-max-iterations s))
      (define new-cache-length (* 4 width height))
      (define new-cache (make-shared-bytes new-cache-length 50))

      (mandelbrot!
       new-cache
       new-cache-length
       center-real
       center-imaginary
       width
       height
       zoom
       max-iterations)

      (update-state
       s
       #:cache new-cache
       #:cache-length new-cache-length
       #:cache-needs-update #f))

    (define (zoom s factor)
      (update-state
       s
       #:zoom (* (state-zoom s) factor)
       #:cache-needs-update #t))

    (define (update-bitmap s)
      (define width (state-width s))
      (define height (state-height s))
      (define cache (state-cache s))
      (define new-bitmap (make-object bitmap% width height))

      (send new-bitmap set-argb-pixels 0 0 width height cache)

      (update-state s #:bitmap new-bitmap))

    (define (draw-cache s)
      (define bitmap (state-bitmap s))
      (define dc (send this get-dc))

      (send dc draw-bitmap bitmap 0 0))

    (define/override (on-event event)
      (cond [(send event button-down? 'left)
             (define new-state
               (update-state
                (update-center
                 state
                 (send event get-x)
                 (send event get-y))
                #:cache-needs-update #t))
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
             (set! state new-state)
             (draw-cache state)]
            [else (draw-cache state)]))

    (define/override (on-size new-width new-height)
      (define new-state
        (update-state
         state
         #:width new-width
         #:height new-height
         #:cache-needs-update #t))
      (set! state new-state))
    ))

(define frame
  (new frame% [label "Mandelbrot Set Viewer"]
       [width 600]
       [height 600]))

(define mandelbrot-canvas (new mandelbrot-canvas% [parent frame]))

(send frame show #t)
