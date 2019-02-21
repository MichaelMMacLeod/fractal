#lang racket/base

(require "mandelbrot.rkt"
         "state.rkt"
         racket/class
         racket/contract/base
         racket/gui/base)

(provide
 (contract-out
  [mandelbrot-canvas%
   (and/c
    (subclass?/c canvas%)
    (class/c
     [get-draw-thread (->m thread?)]
     [set-draw-rate (->m (>=/c 0) void?)]
     (override [on-event (->m (is-a?/c mouse-event%) void?)]
               [on-char (->m (is-a?/c key-event%) void?)]
               [on-paint (->m void?)]
               [on-size (->m dimension-integer? dimension-integer? void?)])))]))

(define mandelbrot-canvas%
  (class canvas%
    (super-new)

    (define state (make-state 600 600))

    (define draw-rate 0.05)

    (define draw-thread (create-draw-thread))

    (define/public (get-draw-thread)
      draw-thread)

    (define/public (create-draw-thread)
      (thread (lambda ()
                (for ([forever (in-naturals)])
                  (redraw-bitmap!/s state)
                  (send this on-paint)
                  (sleep draw-rate)))))

    (define/public (set-draw-rate new-draw-rate)
      (set! draw-rate new-draw-rate))

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
    ))
