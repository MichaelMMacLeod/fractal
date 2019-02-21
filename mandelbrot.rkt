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

(define (mandelbrot-point->color the-real-part the-imaginary-part max-iterations)
  (get-color (escape-time the-real-part the-imaginary-part max-iterations)))

(define (index->mandelbrot-point
         index
         width
         height
         center-real
         center-imaginary
         zoom)
  (define x (remainder index width))
  (define y (quotient index width))

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
  (values the-real-part the-imaginary-part))

(define (insert-color! bytestring index color)
  (bytes-set! bytestring index (argb-color-a color))
  (bytes-set! bytestring (+ index 1) (argb-color-r color))
  (bytes-set! bytestring (+ index 2) (argb-color-g color))
  (bytes-set! bytestring (+ index 3) (argb-color-b color)))

(define (mandelbrot!
         bytestring
         start-index
         end-index
         center-real
         center-imaginary
         width
         height
         zoom
         max-iterations)
  (for ([index (in-range (quotient start-index 4)
                         (quotient end-index 4))])
    (define-values (the-real-part the-imaginary-part)
      (index->mandelbrot-point
       index
       width
       height
       center-real
       center-imaginary
       zoom))

    (define color
      (mandelbrot-point->color the-real-part the-imaginary-part max-iterations))

    (insert-color! bytestring (* 4 index) color)))

(struct worker-message
  (id
   bytestring
   start-index
   end-index
   center-real
   center-imaginary
   width
   heigth
   zoom
   max-iterations)
  #:prefab)

(define (create-workers count)
  (for/list ([worker-number (in-range count)])
    (place
     channel
     (for ([forever (in-naturals)])
       (define input (place-channel-get channel))
       (match input
         [(worker-message
           id
           bytestring
           start-index
           end-index
           center-real
           center-imaginary
           width
           height
           zoom
           max-iterations)
          (mandelbrot!
           bytestring
           start-index
           end-index
           center-real
           center-imaginary
           width
           height
           zoom
           max-iterations)])))))
