#lang racket/base

(require "state.rkt"
         racket/class
         racket/contract/base
         racket/gui/base)

(provide
 (contract-out
  [fractal-canvas%
   (and/c
    (subclass?/c canvas%)
    (class/c
     [get-draw-thread (->m thread?)]
     [set-draw-rate (->m (>=/c 0) void?)]
     (override [on-event (->m (is-a?/c mouse-event%) void?)]
               [on-char (->m (is-a?/c key-event%) void?)]
               [on-paint (->m void?)]
               [on-size (->m dimension-integer? dimension-integer? void?)])))]))

(define fractal-canvas%
  (class canvas%
    (super-new)

    (define state (make-state 600 600
                              "./iterators/mandelbrot.rkt"
                              "./painters/grayscale.rkt"
                              (hash 'max-iterations 500)
                              (hash)))

    (define draw-rate 0.05)

    (define draw-thread (create-draw-thread))

    (define/public (get-draw-thread) draw-thread)

    (define/public (create-draw-thread)
      (thread (lambda ()
                (for ([forever (in-naturals)])
                  (redraw-bitmap!/state state)
                  (send this on-paint)
                  (sleep draw-rate)))))

    (define/public (set-draw-rate new-draw-rate)
      (set! draw-rate new-draw-rate))

    (define/override (on-event event)
      (cond [(send event button-down? 'left)
             (define x (send event get-x))
             (define y (send event get-y))
             (set! state (move-center/state state x y))
             (redraw-cache!/state state)]
            [else (void)]))

    (define/override (on-char event)
      (cond [(eq? #\i (send event get-key-code))
             (set! state (zoom/state state 0.9))
             (redraw-cache!/state state)]
            [(eq? #\o (send event get-key-code))
             (set! state (zoom/state state 1.1))
             (redraw-cache!/state state)]))

    (define/override (on-paint)
      (define dc (send this get-dc))
      (send dc draw-bitmap (state-bitmap state) 0 0))

    (define/override (on-size new-width new-height)
      (set! state (resize/state state new-width new-height))
      (redraw-cache!/state state))))
