#lang racket/base

(require "gui.rkt"
         racket/class
         racket/gui/base)

(define frame
  (new frame% [label "Mandelbrot Set Viewer"]
       [width 600]
       [height 600]))

(define fractal-canvas (new fractal-canvas% [parent frame]))
(send fractal-canvas focus)

(send frame show #t)
