#lang racket/base

(require "define-iterator.rkt")

(define-iterator (mandelbrot [center-real : Float]
                             [center-imaginary : Float]
                             [max-iterations : Integer])
  (let loop ([z-real : Float 0.0]
             [z-imaginary : Float 0.0]
             [iterations : Float 0.0]
             [square : Float 0.0])
    (cond [(or (> iterations 1.0)
               (> square 4.0))
           (color iterations iterations iterations 1.0)]
          [else
           (define z-real-square : Float (* z-real z-real))
           (define z-imaginary-square : Float (* z-imaginary z-imaginary))
           (loop (+ center-real (- z-real-square z-imaginary-square))
                 (+ center-imaginary (* 2.0 (* z-real z-imaginary)))
                 (+ iterations 0.005)
                 (+ z-real-square z-imaginary-square))])))
