#lang at-exp racket/base

(require "fractal-state.rkt"
         "glsl/iter.rkt"
         ffi/vector
         opengl
         opengl/util
         racket/class
         racket/contract/base
         racket/flonum
         racket/format
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

(define (draw-triangles glsl-program
                        fractal-center-real
                        fractal-center-imaginary
                        width
                        height
                        zoom)
  (define buffer (u32vector-ref (glGenBuffers 1) 0))
  (define position-data (f32vector -1.0 -1.0 0.0
                                   1.0 -1.0 0.0
                                   -1.0 1.0 0.0
                                   -1.0 1.0 0.0
                                   1.0 -1.0 0.0
                                   1.0 1.0 0.0))
  (glClearColor 0.0 0.0 0.0 0.0)
  (glClear GL_COLOR_BUFFER_BIT)

  (glUniform1f (glGetUniformLocation glsl-program "fractal_center_real")
               fractal-center-real)
  (glUniform1f (glGetUniformLocation glsl-program "fractal_center_imaginary")
               fractal-center-imaginary)
  (glUniform1f (glGetUniformLocation glsl-program "width") width)
  (glUniform1f (glGetUniformLocation glsl-program "height") height)
  (glUniform1f (glGetUniformLocation glsl-program "zoom") zoom)

  (glBindBuffer GL_ARRAY_BUFFER buffer)
  (glBufferData GL_ARRAY_BUFFER
                (* 18 (gl-type-sizeof GL_FLOAT))
                (gl-vector->cpointer position-data)
                GL_STATIC_DRAW)
  (glGenVertexArrays 1)
  (glBindVertexArray 1)
  (glEnableVertexAttribArray 0)
  (glBindBuffer GL_ARRAY_BUFFER buffer)
  (glVertexAttribPointer 0 3 GL_FLOAT #f 0 #f)
  (glBindVertexArray 1)
  (glDrawArrays GL_TRIANGLES 0 6))

(define (resize width height)
  (glViewport 0 0 width height))

(define opengl-fractal-canvas%
  (class canvas%
    (inherit with-gl-context swap-gl-buffers)

    (init-field fragment-shader)

    (define fractal-center-real 0.0)

    (define fractal-center-imaginary 0.0)

    (define zoom 0.005)

    (define gl-config (new gl-config%))
    (send gl-config set-legacy? #f)

    (define glsl-program #f)

    (define vertex-shader
      @~a{#version 420

          uniform float fractal_center_real;
          uniform float fractal_center_imaginary;
          uniform float width;
          uniform float height;
          uniform float zoom;

          in vec2 vertex_position;

          out float center_real;
          out float center_imaginary;

          void main(void)
          {
           center_real = fractal_center_real + zoom * vertex_position.x * width;
           center_imaginary = - fractal_center_imaginary + zoom * vertex_position.y * height;

           gl_Position = vec4(vertex_position, 1.0, 1.0);
          }
          })

    (super-new [style '(gl no-autoclear)]
               [gl-config gl-config])

    (define/override (on-event event)
      (cond [(send event button-down? 'left)
             (define x (send event get-x))
             (define y (send event get-y))
             (define width (send this get-width))
             (define height (send this get-height))
             (define new-center-real
               (fl+ fractal-center-real
                    (fl* 2.0
                         (fl* zoom
                              (fl- (->fl x)
                                   (fl/ (->fl width)
                                        2.0))))))
             (define new-center-imaginary
               (fl+ fractal-center-imaginary
                    (fl* 2.0
                         (fl* zoom
                              (fl- (->fl y)
                                   (fl/ (->fl height)
                                        2.0))))))
             (set! fractal-center-real new-center-real)
             (set! fractal-center-imaginary new-center-imaginary)
             (send this refresh)]))

    (define/override (on-char event)
      (define code (send event get-key-code))
      (cond [(eq? #\i code)
             (set! zoom (fl* zoom 0.95))
             (send this refresh)]
            [(eq? #\o code)
             (set! zoom (fl* zoom 1.15))
             (send this refresh)]))

    (define/override (on-paint)
      (with-gl-context
        (lambda ()
          (unless glsl-program
            (set! glsl-program
                  (create-program
                   (load-shader (open-input-string vertex-shader) GL_VERTEX_SHADER)
                   (load-shader (open-input-string fragment-shader) GL_FRAGMENT_SHADER)))
            (glUseProgram glsl-program))
          (draw-triangles glsl-program
                          fractal-center-real
                          fractal-center-imaginary
                          (->fl (send this get-width))
                          (->fl (send this get-height))
                          zoom)
          (swap-gl-buffers))))

    (define/override (on-size width height)
      (with-gl-context (lambda () (resize width height))))))

#;(define frame (new frame% [label "OpenGL Canvas Test"]
                   [width 600]
                   [height 600]))

#;(send frame show #t)

#;(define fragment-shader
  (compile-glsl-program (mandelbrot [center_real : Float] [center_imaginary : Float])
    (let loop ([z_real : Float 0.0]
               [z_imaginary : Float 0.0]
               [iterations : Float 0.0]
               [sq : Float 0.0])
      (cond [(or (> iterations 1.0)
                 (> sq 4.0))
             (color iterations iterations iterations 1.0)]
            [else
             (define z_real_square : Float (* z_real z_real))
             (define z_imaginary_square : Float (* z_imaginary z_imaginary))
             (loop (+ center_real (- z_real_square z_imaginary_square))
                   (+ (* 2.0 (* z_real z_imaginary)) center_imaginary)
                   (+ iterations 0.005)
                   (+ z_real_square z_imaginary_square))]))))

#;(define opengl-fractal-canvas
  (new opengl-fractal-canvas% [parent frame] [fragment-shader fragment-shader]))


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
                  (send this refresh)
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
             (send state redraw-cache)]
            [(eq? #\s (send event get-key-code))
             (send state
                   save-bitmap
                   (~a "fractal-" (current-milliseconds))
                   'png)]))

    (define/override (on-paint)
      (define dc (send this get-dc))
      (send dc draw-bitmap (send state get-bitmap) 0 0))

    (define/override (on-size new-width new-height)
      (send state resize new-width new-height)
      (send state redraw-cache))))
