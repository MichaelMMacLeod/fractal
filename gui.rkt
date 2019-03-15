#lang at-exp racket/base

(require (submod "iterators/mandelbrot.rkt" iterator:opengl)
         ffi/vector
         opengl
         opengl/util
         racket/class
         racket/contract/base
         racket/flonum
         racket/format
         racket/gui/base)

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
             (set! zoom (fl* zoom 1.05))
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

(define frame (new frame% [label "OpenGL Canvas Test"]
                   [width 600]
                   [height 600]))

(send frame show #t)

(define fragment-shader iterator)

(define opengl-fractal-canvas
  (new opengl-fractal-canvas% [parent frame] [fragment-shader fragment-shader]))

(displayln fragment-shader)

#;(define fragment-shader
  @~a{#version 420

      in float center_real;
      in float center_imaginary;

      float loop(float z_real, float z_imaginary, float iterations, float sq)
      {
       while (true)
       {
        if ((iterations > 1.0) || (sq > 4.0))
        {
         return iterations;
        }
        else
        {
         float z_real_square = z_real * z_real;
         float z_imaginary_square = z_imaginary * z_imaginary;

         float z_real_1 = center_real + z_real_square - z_imaginary_square;
         float z_imaginary_1 = center_imaginary + 2.0 * z_real * z_imaginary;
         float iterations_1 = iterations + 0.005;
         float sq_1 = z_real_square + z_imaginary_square;

         z_real = z_real_1;
         z_imaginary = z_imaginary_1;
         iterations = iterations_1;
         sq = sq_1;
        }
       }
      }

      void main(void)
      {
       float iterations = loop(0.0, 0.0, 0.0, 0.0);

       gl_FragColor = vec4(iterations, iterations, iterations, 1.0);
      }
      })
