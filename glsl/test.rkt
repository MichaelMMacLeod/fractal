#lang racket/gui
(require opengl opengl/util racket/flonum (for-syntax racket/syntax))
 
(define (resize w h)
  (glViewport 0 0 w h))

(define shader-source:fragment
  #<<END
varying float xpos;
varying float ypos;

void main(void) {
    float z_real = 0.0;
    float z_imaginary = 0.0;
    float iterations = 0.0;
    float a = xpos;
    float bi = ypos;
    float sq = 0.0;
    while (iterations < 1.0 && sq < 4.0) {
        float z_real_square = z_real * z_real;
        float z_imaginary_square = z_imaginary * z_imaginary;
        float z_real_new = z_real_square - z_imaginary_square + a;
        float z_imaginary_new = 2.0 * z_real * z_imaginary + bi;
        z_real = z_real_new;
        z_imaginary = z_imaginary_new;
        iterations = iterations + 0.005;
        sq = z_real_square + z_imaginary_square;
    }
    gl_FragColor = vec4(iterations, iterations, iterations, 1.0);
}
END
  )

(define shader-source:vertex
  #<<END
uniform float real;
uniform float imag;
uniform float w;
uniform float h;
uniform float zoom;

varying float xpos;
varying float ypos;

void main(void)
{
  xpos = real + zoom * gl_Vertex.x * w / 2;
  ypos = imag + zoom * gl_Vertex.y * h / 2;

  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
}
END
  ) 

(define-syntax (glSetUniformLocations stx)
  (syntax-case stx ()
    [(_ program (var type value) ...)
     (with-syntax ([(function-name ...)
                    (for/list ([t (in-list (syntax->list #'(type ...)))])
                      (format-id t
                                 #:source t
                                 "glUniform~a"
                                 (syntax-e t)))])
       #'(begin
          (function-name (glGetUniformLocation program
                                               (symbol->string 'var))
                         value)
          ...))]))
 
(define (draw-opengl prg r i w h z)
  (glClearColor 0.0 0.0 0.0 0.0)
  (glClear GL_COLOR_BUFFER_BIT)

  (glSetUniformLocations prg
    (real 1f r)
    (imag 1f i)
    (zoom 1f z)
    (w 1f w)
    (h 1f h))

  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
  (glOrtho 0.0 1.0 0.0 1.0 0.0 1.0)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (glOrtho -1.0 1.0 -1.0 1.0 0.0 1.0)

  (glBegin GL_QUADS)
  (glVertex2f 0.0 0.0)
  (glVertex2f 0.0 h)
  (glVertex2f w h)
  (glVertex2f w 0.0)
  (glEnd))
 
 
(define my-canvas%
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)
    (define the-program #f)

    (define real 0.0)
    (define imag 0.0)
    (define zoom 1.0)

    (define/override (on-event event)
      (cond [(send event button-down? 'left)
             (define x (send event get-x))
             (define y (send event get-y))
             (define width (send this get-width))
             (define height (send this get-height))
             (define new-center-real
               (fl+ real
                    (fl* zoom
                         (fl- (->fl x)
                              (fl/ (->fl width)
                                   2.0)))))
             (define new-center-imaginary
               (fl+ imag
                    (fl* zoom
                         (fl- (->fl y)
                              (fl/ (->fl height)
                                   2.0)))))
             (set! real new-center-real)
             (set! imag new-center-imaginary)
             (send this refresh)]))

    (define/override (on-char event)
      (define code (send event get-key-code))
      (cond [(eq? #\i code)
             (set! zoom (fl* zoom 0.95))
             (send this refresh)]
            [(eq? #\o code)
             (set! zoom (fl* zoom 1.15))
             (send this refresh)]
            [(eq? #\a code)
             (set! real (fl- real (fl* zoom 0.01)))
             (send this refresh)]
            [(eq? #\d code)
             (set! real (fl+ real (fl* zoom 0.01)))
             (send this refresh)]
            [(eq? #\w code)
             (set! imag (fl+ imag (fl* zoom 0.01)))
             (send this refresh)]
            [(eq? #\s code)
             (set! imag (fl- imag (fl* zoom 0.01)))
             (send this refresh)]))
 
    (define/override (on-paint)
      (with-gl-context
          (λ()
            (unless the-program
              (set! the-program
                    (create-program 
                      (load-shader (open-input-string shader-source:fragment)
                                   GL_FRAGMENT_SHADER)
                      (load-shader (open-input-string shader-source:vertex)
                                   GL_VERTEX_SHADER)))
              (glUseProgram the-program))
            (draw-opengl the-program 
                         real 
                         imag 
                         (->fl (send this get-width)) 
                         (->fl (send this get-height))
                         zoom) 
            (swap-gl-buffers))))
    (define/override (on-size width height)
      (with-gl-context (λ() (resize width height))))
    (super-instantiate () (style '(gl no-autoclear)))))
 
(define win (new frame% [label "Racket Rosetta Code OpenGL example"]
                        [min-width 600] [min-height 600]))
(define gl (new my-canvas% [parent win]))
(send gl focus)
 
(send win show #t)
