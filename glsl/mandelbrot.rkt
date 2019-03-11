#lang racket

(require "iter.rkt")

(define-iterator (mandelbrot [xpos : Float] [ypos : Float])
  (let loop ([z_real : Float 0.0]
             [z_imaginary : Float 0.0]
             [iterations : Float 0.0]
             [sq : Float 0.0])
    (cond [(or (> iterations 1.0)
               (> sq 4.0))
           (define red : Float iterations)
           (define green : Float iterations)
           (define blue : Float iterations)
           (define alpha : Float 1.0)
           (color red green blue alpha)]
          [else
           (define z_real_square : Float
             (* z_real z_real))
           (define z_imaginary_square : Float
             (* z_imaginary z_imaginary))
           (loop (+ xpos (- z_real_square z_imaginary_square))
                 (+ (* 2.0 z_real z_imaginary) ypos)
                 (+ iterations 0.005)
                 (+ z_real_square z_imaginary_square))])))
