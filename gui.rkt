#lang racket/base

(require "object-state.rkt"
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

    (init-field state
                [draw-rate 0.01])

    (define draw-thread (create-draw-thread))

    (define/public (get-draw-thread) draw-thread)

    (define/public (create-draw-thread)
      (thread (lambda ()
                (for ([forever (in-naturals)])
                  (send state redraw-bitmap)
                  (send this on-paint)
                  (sleep draw-rate)))))

    (define/public (set-draw-rate new-draw-rate)
      (set! draw-rate new-draw-rate))

    (define/override (on-event event)
      (cond [(send event button-down? 'left)
             (define x (send event get-x))
             (define y (send event get-y))
             (send state move-center x y)
             (send state redraw-cache)]
            [else (void)]))

    (define/override (on-char event)
      (cond [(eq? #\i (send event get-key-code))
             (send state zoom-in)
             (send state redraw-cache)]
            [(eq? #\o (send event get-key-code))
             (send state zoom-out)
             (send state redraw-cache)]))

    (define/override (on-paint)
      (define dc (send this get-dc))
      (send dc draw-bitmap (send state get-bitmap) 0 0))

    (define/override (on-size new-width new-height)
      (send state resize new-width new-height)
      (send state redraw-cache))))
