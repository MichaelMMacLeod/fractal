#lang racket/base

(require "gui.rkt"
         racket/class
         racket/gui/base)

(define frame
  (new frame% [label "Mandelbrot Set Viewer"]
       [width 600]
       [height 600]))

(define mandelbrot-canvas (new mandelbrot-canvas% [parent frame]))
(send mandelbrot-canvas focus)

(send frame show #t)
