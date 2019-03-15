#lang racket/base

(require "../glsl/iter.rkt")

(define-iterator (mandelbrot [center_real : Float]
                             [center_imaginary : Float]
                             [max_iterations : Integer])
  (let loop ([z_real : Float 0.0]
             [z_imaginary : Float 0.0]
             [iterations : Float 0.0]
             [square : Float 0.0])
    (cond [(or (> iterations 1.0)
               (> square 4.0))
           (color iterations iterations iterations 1.0)]
          [else
           (define z_real_square : Float (* z_real z_real))
           (define z_imaginary_square : Float (* z_imaginary z_imaginary))
           (loop (+ center_real (- z_real_square z_imaginary_square))
                 (+ center_imaginary (* 2.0 (* z_real z_imaginary)))
                 (+ iterations 0.005)
                 (+ z_real_square z_imaginary_square))])))
