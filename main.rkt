#lang racket/base

(require "fractal-state.rkt"
         "gui.rkt"
         "parameters.rkt"
         racket/class
         racket/cmdline
         racket/flonum
         racket/format
         racket/future
         racket/list
         racket/sequence
         racket/gui/base)

(command-line
 #:program "fractal"
 #:once-each
 [("-x" "--width")
  width
  "The fractal's initial width."
  (the-width (string->number width))]
 [("-y" "--height")
  height
  "The fractal's initial height."
  (the-height (string->number height))]
 [("-i" "--iterator-path")
  iterator-path
  "The iterator used for computing the fractal."
  (the-iterator-path iterator-path)]
 [("-p" "--painter-path")
  painter-path
  "The painter used for coloring the fractal."
  (the-painter-path painter-path)]
 [("-w" "--worker-count")
  worker-count
  "The maximum number of parallel drawing processes."
  (the-worker-count (string->number worker-count))]
 [("-a" "--center-real")
  center-real
  "The real component of the fractal's complex center."
  (the-center-real (string->number center-real))]
 [("-b" "--center-imaginary")
  center-imaginary
  "The imaginary component of the fractal's complex center."
  (the-center-imaginary (string->number center-imaginary))]
 [("-z" "--zoom")
  zoom
  "The fractal's initial zoom."
  (the-zoom (string->number zoom))]
 [("-f" "--zoom-factor")
  zoom-factor
  "The amount the fractal zooms on any zoom in or out command."
  (the-zoom-factor (string->number zoom-factor))]
 [("-d" "--default-cache-value")
  default-cache-value
  "The color visible behind the fractal before it has been drawn."
  (the-default-cache-value (string->number default-cache-value))]
 #:args info-pairs
 (let ([info-pair-count (length info-pairs)])
   (cond [(zero? info-pair-count)
          (void)]
         [(even? info-pair-count)
          (the-info
           (for/fold ([info (the-info)])
                     ([key+value (in-slice 2 info-pairs)])
             (hash-set info
                       (string->symbol (first key+value))
                       (read (open-input-string (second key+value))))))]
         [else
          (displayln (~a "Expected zero or more <key> <value> pairs, but was given"
                         info-pairs))])))

(define (make-exact-integer n)
  (cond [(exact? n) (floor n)]
        [else (inexact->exact (inexact->exact n))]))

(unless (the-width)
  (the-width 600))

(unless (the-height)
  (the-height 600))

(unless (the-iterator-path)
  (the-iterator-path "./iterators/mandelbrot.rkt"))

(unless (the-painter-path)
  (the-painter-path "./painters/rainbow.rkt"))

(unless (the-worker-count)
  (the-worker-count (processor-count)))

(unless (the-center-real)
  (the-center-real 0.0))

(unless (the-center-imaginary)
  (the-center-imaginary 0.0))

(unless (the-zoom)
  (the-zoom 0.05))

(unless (the-zoom-factor)
  (the-zoom-factor 0.05))

(unless (the-default-cache-value)
  (the-default-cache-value 50))

(cond [(the-info)
       (cond [(hash-ref (the-info) 'max-iterations #f)
              (void)]
             [else
              (the-info (hash-set (the-info) 'max-iterations 500))])]
      [else
       (the-info (hash 'max-iterations 500))])

(define state
  (new fractal-state%
       [width (the-width)]
       [height (the-height)]
       [iterator-path (the-iterator-path)]
       [painter-path (the-painter-path)]
       [info (the-info)]
       [worker-count (the-worker-count)]
       [center-real (the-center-real)]
       [center-imaginary (the-center-imaginary)]
       [zoom (the-zoom)]
       [zoom-factor (the-zoom-factor)]
       [default-cache-value (the-default-cache-value)]))

(define frame
  (new frame%
       [label "Fractal Viewer"]
       [width (the-width)]
       [height (the-height)]))

(define fractal-canvas
  (new fractal-canvas%
       [parent frame]
       [state state]))

(send frame show #t)
(send fractal-canvas focus)
