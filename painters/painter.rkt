#lang racket/base

(provide (struct-out argb-color))

(struct argb-color (a r g b) #:prefab)
