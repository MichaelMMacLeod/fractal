#lang racket/base

(require "mandelbrot.rkt"
         "state.rkt"
         racket/class
         racket/gui/base
         )

(provide (all-defined-out))

(define mandelbrot-canvas%
  (class canvas%
    (super-new)

    (define state (make-state 600 600))

    (define/override (on-event event)
      (cond [(send event button-down? 'left)
             (define x (send event get-x))
             (define y (send event get-y))
             (set! state (move-center/s state x y))
             (redraw-cache!/s state)]
            [else (void)]))

    (define/override (on-char event)
      (cond [(eq? #\i (send event get-key-code))
             (set! state (zoom/s state 0.9))
             (redraw-cache!/s state)]
            [(eq? #\o (send event get-key-code))
             (set! state (zoom/s state 1.1))
             (redraw-cache!/s state)]))

    (define/override (on-paint)
      (define dc (send this get-dc))
      (send dc draw-bitmap (state-bitmap state) 0 0))

    (define/override (on-size new-width new-height)
      (set! state (resize/s state new-width new-height))
      (redraw-cache!/s state))

    (thread (lambda ()
              (for ([forever (in-naturals)])
                (redraw-bitmap!/s state)
                (send this on-paint)
                (sleep 0.05))))))
