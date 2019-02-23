#lang racket/base

(require "gui.rkt"
         racket/class
         racket/cmdline
         racket/flonum
         racket/future
         racket/gui/base)

(define the-iterator-path (make-parameter "mandelbrot"))
(define the-painter-path (make-parameter "grayscale"))
(define the-iterator-info (make-parameter (hash 'max-iterations 500)))
(define the-painter-info (make-parameter (hash)))
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
 #:multi
 (["-i" "--iterator-info"] key value 
  ""
  (the-iterator-info (hash-set (the-iterator-info) 
                               (string->symbol key) 
                               (read (open-input-string value)))))
 (["-p" "--painter-info"] key value
  ""
  (the-painter-info (hash-set (the-painter-info) 
                              (string->symbol key)
                              (read (open-input-string value))))))

(cond [(equal? (the-iterator-path) "mandelbrot")
       (the-iterator-path "./iterators/mandelbrot.rkt")]
      [(equal? (the-iterator-path) "julia")
       (the-iterator-path "./iterators/julia.rkt")]
      [else (void)])

(cond [(equal? (the-painter-path) "grayscale")
       (the-painter-path "./painters/grayscale.rkt")]
      [(equal? (the-painter-path) "rgb")
       (the-painter-path "./painters/rgb.rkt")]
      [else (void)])

(define frame
  (new frame% 
       [label "Mandelbrot Set Viewer"]
       [width (the-width)]
       [height (the-height)]))

(define fractal-canvas 
  (new fractal-canvas% 
       [parent frame]
       [iterator-path (the-iterator-path)]
       [painter-path (the-painter-path)]
       [iterator-info (the-iterator-info)]
       [painter-info (the-painter-info)]
       [center-real (the-center-real)]
       [center-imaginary (the-center-imaginary)]
       [zoom (the-zoom)]
       [worker-count (the-worker-count)]
       [draw-rate (the-draw-rate)]))

(send fractal-canvas focus)

(send frame show #t)
