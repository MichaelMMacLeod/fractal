#lang racket

(require racket/flonum)

(provide (all-defined-out))

(define (escape-time center-real center-imaginary max-iterations)
  (let loop ([z-real 0.0]
             [z-imaginary 0.0]
             [iterations 0])
    (define z-real-square (fl* z-real z-real))
    (define z-imaginary-square (fl* z-imaginary z-imaginary))
    (cond [(or (fl>= (flsqrt (fl+ z-real-square
                                  z-imaginary-square))
                     2.0)
               (>= iterations max-iterations))
           iterations]
          [else (loop (fl+ (fl- z-real-square
                                z-imaginary-square)
                           center-real)
                      (fl+ (fl* 2.0
                                (fl* z-real z-imaginary))
                           center-imaginary)
                      (add1 iterations))])))

(struct argb-color (a r g b))

(define (get-color iterations)
  (define v (modulo (* iterations 25) 255))
  (argb-color 255 v v v))

(define (mandelbrot!
         bytestring
         bytestring-length
         center-real
         center-imaginary
         width
         height
         zoom
         max-iterations)
  (for ([i (in-range 0 bytestring-length)]
        [position (in-range 0 bytestring-length 4)])
    (define x (remainder i width))
    (define y (quotient i width))

    (define the-real-part
      (fl+ center-real
           (fl* zoom
                (fl- (->fl x)
                     (fl/ (->fl width)
                          2.0)))))

    (define the-imaginary-part
      (fl+ center-imaginary
           (fl* zoom
                (fl- (->fl y)
                     (fl/ (->fl height)
                          2.0)))))
    (define iterations
      (escape-time the-real-part
                   the-imaginary-part
                   max-iterations))

    (define color (get-color iterations))

    (bytes-set! bytestring position (argb-color-a color))
    (bytes-set! bytestring (+ position 1) (argb-color-r color))
    (bytes-set! bytestring (+ position 2) (argb-color-g color))
    (bytes-set! bytestring (+ position 3) (argb-color-b color))))
