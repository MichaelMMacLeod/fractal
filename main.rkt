#lang racket/base

(require "gui.rkt"
         "fractal-state.rkt"
         racket/class
         racket/cmdline
         racket/flonum
         racket/format
         racket/future
         racket/list
         racket/sequence
         racket/gui/base)

(define the-iterator-path (make-parameter "mandelbrot"))
(define the-painter-path (make-parameter "rainbow"))
(define the-info (make-parameter (hash 'max-iterations 500)))
(define the-width (make-parameter 600))
(define the-height (make-parameter 600))
(define the-center-real (make-parameter 0.0))
(define the-center-imaginary (make-parameter 0.0))
(define the-zoom (make-parameter 0.005))
(define the-worker-count (make-parameter (processor-count)))
(define the-draw-rate (make-parameter 0.01))

(define (make-fl x)
  (cond [(exact? x)
         (->fl x)]
        [(flonum? x)
         x]
        [else (error 'make-fl x)]))

(command-line
 #:program "fractal"
 #:once-each
 [("-I" "--iterator") iterator-path
  ""
  (the-iterator-path iterator-path)]
 [("-P" "--painter") painter-path
  ""
  (the-painter-path painter-path)]
 [("-s" "--size") width height
  ""
  (the-width (read (open-input-string width)))
  (the-height (read (open-input-string height)))]
 [("-c" "--center") center-real center-imaginary
  ""
  (the-center-real (make-fl (read (open-input-string center-real))))
  (the-center-imaginary (make-fl (read (open-input-string center-imaginary))))]
 [("-z" "--zoom") zoom
  ""
  (the-zoom (make-fl (read (open-input-string zoom))))]
 [("-w" "--worker-count") worker-count
  ""
  (the-worker-count (read (open-input-string worker-count)))]
 [("-d" "--draw-rate") draw-rate
  ""
  (the-draw-rate (read (open-input-string draw-rate)))]
 #:args extra-info
 (cond [(zero? (length extra-info)) (void)]
       [(even? (length extra-info))
        (the-info
         (for/fold ([h (the-info)])
                   ([key-value (in-slice 2 extra-info)])
           (hash-set h
                     (string->symbol (first key-value))
                     (read (open-input-string (second key-value))))))]
       [else (displayln (~a "Expected an even number of info items, but was given "
                            (length extra-info)
                            " in "
                            (~a extra-info)))
             (exit 1)]))

(the-iterator-path
  (case (the-iterator-path)
    [("mandelbrot") "./iterators/mandelbrot.rkt"]
    [("julia") "./iterators/julia.rkt"]
    [("burning-ship") "./iterators/burning-ship.rkt"]
    [else (the-iterator-path)]))

(the-painter-path
  (case (the-painter-path)
    [("rainbow") "./painters/rainbow.rkt"]
    [("grayscale") "./painters/grayscale.rkt"]
    [("rgb") "./painters/rgb.rkt"]
    [else (the-painter-path)]))

(define frame
  (new frame%
       [label "Mandelbrot Set Viewer"]
       [width (the-width)]
       [height (the-height)]))

(define state
  (new fractal-state%
       [width 600]
       [height 600]
       [iterator-path (the-iterator-path)]
       [painter-path (the-painter-path)]
       [info (the-info)]))

(define fractal-canvas
  (new fractal-canvas% [parent frame] [state state]))

(send fractal-canvas focus)

(send frame show #t)
