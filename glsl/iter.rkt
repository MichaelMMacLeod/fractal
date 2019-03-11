#lang racket

(require syntax/parse
         (for-syntax racket
                     racket/syntax
                     syntax/parse
                     syntax/to-string
                     syntax/strip-context))

(provide define-iterator)

(begin-for-syntax
  (define-syntax-class glsl-arithmetic-expression
    #:description "glsl arithmetic expression"
    #:datum-literals (+ - * /)
    (pattern var:id
             #:with expr #'var)
    (pattern n:number
             #:with expr #'n)
    (pattern (+ arg:glsl-arithmetic-expression ...)
             #:with expr #'(+ arg ...))
    (pattern (- arg:glsl-arithmetic-expression ...)
             #:with expr #'(- arg ...))
    (pattern (* arg:glsl-arithmetic-expression ...)
             #:with expr #'(* arg ...))
    (pattern (/ arg:glsl-arithmetic-expression ...)
             #:with expr #'(/ arg ...)))

  (define-syntax-class glsl-conditional-expression
    #:description "glsl conditional expression"
    #:datum-literals (or and not < > <= >= =)
    (pattern b:boolean
             #:with expr #'b)
    (pattern glsl-arith:glsl-arithmetic-expression
             #:with expr #'glsl-arith)
    (pattern (or arg:glsl-conditional-expression ...)
             #:with expr #'(or arg ...))
    (pattern (and arg:glsl-conditional-expression ...)
             #:with expr #'(and arg ...))
    (pattern (not arg:glsl-conditional-expression)
             #:with expr #'(not arg))
    (pattern (< arg:glsl-arithmetic-expression ...)
             #:with expr #'(< arg ...))
    (pattern (> arg:glsl-arithmetic-expression ...)
             #:with expr #'(> arg ...))
    (pattern (<= arg:glsl-arithmetic-expression ...)
             #:with expr #'(<= arg ...))
    (pattern (>= arg:glsl-arithmetic-expression ...)
             #:with expr #'(>= arg ...))
    (pattern (= arg:glsl-arithmetic-expression ...)
             #:with expr #'(= arg ...)))

  (define-syntax-class glsl-type
    #:description "glsl type"
    #:datum-literals (float int)
    (pattern float)
    (pattern int))

  (define-syntax-class varying
    #:description "glsl varying variable declaration"
    (pattern [type:glsl-type name:id]))

  (define-syntax-class condition
    (pattern x))

  (define-syntax-class definition
    (pattern [type:glsl-type name:id binding:glsl-arithmetic-expression]))

  (define-syntax-class update
    (pattern [name:id binding:glsl-arithmetic-expression]))

  (define-syntax-class color
    (pattern [red:glsl-arithmetic-expression
              green:glsl-arithmetic-expression
              blue:glsl-arithmetic-expression
              alpha:glsl-arithmetic-expression]
             #:with (colors ...) #'(red green blue alpha)))

  (define-syntax-class main-function
    (pattern ((pre-loop-def:definition ...)
              (end-loop-expr:glsl-conditional-expression ...)
              (defs:definition ...)
              (var-assignment:update ...)
              c:color)
             #:with (pre-loop-var ...) #'(pre-loop-def.name ...)
             #:with (pre-loop-var.type ...) #'(pre-loop-def.type ...)
             #:with (pre-loop-var.binding ...) #'(pre-loop-def.binding ...)
             #:with (end-loop.expr ...) #'(end-loop-expr.expr ...)
             #:with (loop-var ...) #'(defs.name ...)
             #:with (loop-var.type ...) #'(defs.type ...)
             #:with (loop-var.binding ...) #'(defs.binding ...)
             #:with (assignment ...) #'(var-assignment.name ...)
             #:with (assignment.binding ...) #'(var-assignment.binding ...)
             #:with (colors ...) #'(c.colors ...)))

  (define-syntax-class fragment-shader
    (pattern ((var:varying ...) main:main-function)
             #:with (arg ...) #'(var.name ...)
             #:with (arg.type ...) #'(var.type ...)
             #:with (pre-loop-var ...) #'(main.pre-loop-var ...)
             #:with (pre-loop-var.type ...) #'(main.pre-loop-var.type ...)
             #:with (pre-loop-var.binding ...) #'(main.pre-loop-var.binding ...)
             #:with (end-loop ...) #'(main.end-loop.expr ...)
             #:with (loop-var ...) #'(main.loop-var ...)
             #:with (loop-var.type ...) #'(main.loop-var.type ...)
             #:with (loop-var.binding ...) #'(main.loop-var.binding ...)
             #:with (assignment ...) #'(main.assignment ...)
             #:with (assignment.binding ...) #'(main.assignment.binding ...)
             #:with (colors ...) #'(main.colors ...))))

(begin-for-syntax
  (define (indent n strings #:spaces [spaces 2])
    (for/list ([s (in-list strings)])
      (string-append (make-string (* spaces n) #\Space) s)))

  (define (type->glsl-type type)
    (match type
      ['Float 'float]
      ['Integer 'int]))

  (define (compile-varying-defs types args)
    (for/list ([type (in-list (syntax->list types))]
               [arg (in-list (syntax->list args))])
      (format "varying ~a ~a;"
              (type->glsl-type (syntax-e type))
              (syntax-e arg))))

  (define (compile-arithmetic-expression c)
    (match c
      [`(+ ,a ,b)
       (compile-infix-expression "+"
                                 (compile-arithmetic-expression a)
                                 (compile-arithmetic-expression b))]
      [`(- ,a ,b)
       (compile-infix-expression "-"
                                 (compile-arithmetic-expression a)
                                 (compile-arithmetic-expression b))]
      [`(* ,a ,b)
       (compile-infix-expression "*"
                                 (compile-arithmetic-expression a)
                                 (compile-arithmetic-expression b))]
      [`(/ ,a ,b)
       (compile-infix-expression "/"
                                 (compile-arithmetic-expression a)
                                 (compile-arithmetic-expression b))]
      [x (~a x)]))

  (define (compile-binding-declarations pre-loop-vars
                                        pre-loop-var-types
                                        pre-loop-var-bindings)
    (for/list ([var (in-list (syntax->list pre-loop-vars))]
               [type (in-list (syntax->list pre-loop-var-types))]
               [binding (in-list (syntax->list pre-loop-var-bindings))])
      (format "~a ~a = ~a;"
              (type->glsl-type (syntax-e type))
              (syntax-e var)
              (compile-arithmetic-expression (syntax->datum binding)))))

  (define (compile-infix-expression op left right)
    (format "(~a ~a ~a)" left op right))

  (define (compile-conditional-expression c)
    (match c
      [`(or ,a ,b)
       (compile-infix-expression "||"
                                 (compile-conditional-expression a)
                                 (compile-conditional-expression b))]
      [`(and ,a ,b)
       (compile-infix-expression "&&"
                                 (compile-conditional-expression a)
                                 (compile-conditional-expression b))]
      [`(not ,a)
       (format "(!~a)" (compile-conditional-expression a))]
      [`(< ,a ,b)
       (compile-infix-expression "<"
                                 (compile-conditional-expression a)
                                 (compile-conditional-expression b))]
      [`(> ,a ,b)
       (compile-infix-expression ">"
                                 (compile-conditional-expression a)
                                 (compile-conditional-expression b))]
      [`(<= ,a ,b)
       (compile-infix-expression "<="
                                 (compile-conditional-expression a)
                                 (compile-conditional-expression b))]
      [`(>= ,a ,b)
       (compile-infix-expression ">="
                                 (compile-conditional-expression a)
                                 (compile-conditional-expression b))]
      [`(= ,a ,b)
       (compile-infix-expression "="
                                 (compile-conditional-expression a)
                                 (compile-conditional-expression b))]
      [x (~a x)]))

  (define (compile-loop-condition end-loop)
    (format
     "while (!(~a)) {"
     (format "~a" (compile-conditional-expression
                   (syntax->datum end-loop)))
     #;(for/fold ([result "false"])
               ([end (in-list (syntax->list end-loops))])
       (displayln end-loops)
       (format "~a || ~a" result (compile-conditional-expression
                                  (syntax->datum end))))))

  (define (compile-assignments assignments assignment-bindings)
    (for/list ([assignment (in-list (syntax->list assignments))]
               [binding (in-list (syntax->list assignment-bindings))])
      (format "~a = ~a;"
              (syntax-e assignment)
              (compile-arithmetic-expression (syntax->datum binding)))))

  (define (~a* xs)
    (apply ~a #:separator "\n" xs))

  (define (compile-colors colors)
    (match (for/list ([x (in-list (syntax->list colors))])
             (compile-arithmetic-expression (syntax->datum x)))
      [(list red green blue alpha)
       (format "gl_FragColor = vec4(~a, ~a, ~a, ~a);"
               red green blue alpha)]))

  (define (compile-main pre-loop-vars
                        pre-loop-var-types
                        pre-loop-var-bindings
                        end-loops
                        loop-vars
                        loop-var-types
                        loop-var-bindings
                        assignments
                        assignment-bindings
                        post-vars
                        post-var-types
                        post-var-bindings
                        colors)
    (~a* `("void main(void) {"
           ,@(indent
              1
              `(,@(compile-binding-declarations pre-loop-vars
                                                pre-loop-var-types
                                                pre-loop-var-bindings)
                ,(compile-loop-condition end-loops)
                ,@(indent
                   1
                   `(,@(compile-binding-declarations loop-vars
                                                     loop-var-types
                                                     loop-var-bindings)
                     ,@(compile-assignments assignments assignment-bindings)))))
           ,@(indent 1 '("}"))
           ,@(indent
              1
              `(,@(compile-binding-declarations post-vars
                                                post-var-types
                                                post-var-bindings)))
           ,@(indent 1 (list (compile-colors colors)))
           "}"))))

(begin-for-syntax
  (define-syntax-class type
    #:description "type"
    #:datum-literals (Float Integer)
    (pattern Float)
    (pattern Integer))

  (define-syntax-class typed-variable
    #:description "typed variable"
    #:datum-literals (:)
    (pattern [var:id : type:type]))

  (define-syntax-class typed-function-header
    #:description "function header with typed variables"
    (pattern ((~describe #:opaque "function name identifier" name:id)
              var:typed-variable ...)
             #:with (arg ...) #'(var.var ...)
             #:with (type ...) #'(var.type ...)))

  (define-syntax-class arithmetic-expression
    #:description "arithmetic expression"
    #:datum-literals (+ - * /)
    (pattern var:id
             #:with expr #'var)
    (pattern n:number
             #:with expr #'n)
    (pattern (+ arg:arithmetic-expression ...)
             #:with expr #'(+ arg ...))
    (pattern (- arg:arithmetic-expression ...)
             #:with expr #'(- arg ...))
    (pattern (* arg:arithmetic-expression ...)
             #:with expr #'(* arg ...))
    (pattern (/ arg:arithmetic-expression ...)
             #:with expr #'(/ arg ...)))

  (define-syntax-class typed-variable-binding
    #:description "typed variable binding"
    #:datum-literals (:)
    (pattern [var:id : type:type binding:arithmetic-expression]))

  (define-syntax-class typed-variable-definition
    #:description "typed variable definition"
    #:datum-literals (define :)
    (pattern (define var:id : type:type value:arithmetic-expression)))

  (define-syntax-class conditional-expression
    #:description "conditional expression"
    #:datum-literals (or and not < > <= >= =)
    (pattern b:boolean
             #:with expr #'b)
    (pattern arith:arithmetic-expression
             #:with expr #'arith)
    (pattern (or arg:conditional-expression ...)
             #:with expr #'(or arg ...))
    (pattern (and arg:conditional-expression ...)
             #:with expr #'(and arg ...))
    (pattern (not arg:conditional-expression)
             #:with expr #'(not arg))
    (pattern (< arg:arithmetic-expression ...)
             #:with expr #'(< arg ...))
    (pattern (> arg:arithmetic-expression ...)
             #:with expr #'(> arg ...))
    (pattern (<= arg:arithmetic-expression ...)
             #:with expr #'(<= arg ...))
    (pattern (>= arg:arithmetic-expression ...)
             #:with expr #'(>= arg ...))
    (pattern (= arg:arithmetic-expression ...)
             #:with expr #'(= arg ...)))

  (define-syntax-class color-expression
    #:description "color expression"
    #:datum-literals (color)
    (pattern (color red:arithmetic-expression
                    green:arithmetic-expression
                    blue:arithmetic-expression
                    alpha:arithmetic-expression)))

  (define-syntax-class test-clause
    #:description "test clause"
    (pattern [test:conditional-expression
              then-body:typed-variable-definition ...
              then-color:color-expression]
             #:with (then ...) #'(then-body.var ...)
             #:with (then.type ...) #'(then-body.type ...)
             #:with (then.binding ...) #'(then-body.value ...)
             #:with color #'then-color
             #:with (colors ...) #'(then-color.red
                              then-color.blue
                              then-color.green
                              then-color.alpha)))

  (define-syntax-class loop-iterate-clause
    #:description "loop iterate clause"
    #:datum-literals (loop)
    (pattern (loop iterate-clause:arithmetic-expression ...)))

  (define-syntax-class else-clause
    #:description "else clause"
    #:datum-literals (else)
    (pattern [else
              else-body:typed-variable-definition ...
              loop-iterate-clause:loop-iterate-clause]
             #:with (var ...) #'(else-body.var ...)
             #:with (type ...) #'(else-body.type ...)
             #:with (binding ...) #'(else-body.value ...)
             #:with iterate #'loop-iterate-clause))

  (define-syntax-class cond-form
    #:description "cond clause"
    #:datum-literals (cond)
    (pattern (cond test-clause:test-clause else-clause:else-clause)
             #:with test #'test-clause.test
             #:with (then ...) #'(test-clause.then ...)
             #:with (then.type ...) #'(test-clause.then.type ...)
             #:with (then.binding ...) #'(test-clause.then.binding ...)
             #:with color #'test-clause.color
             #:with (colors ...) #'(test-clause.colors ...)
             #:with (else ...) #'(else-clause.var ...)
             #:with (else.type ...) #'(else-clause.type ...)
             #:with (else.binding ...) #'(else-clause.binding ...)
             #:with iterate #'else-clause.iterate))

  (define-syntax-class loop-form
    #:description "loop form"
    #:datum-literals (let loop)
    (pattern (let loop (var-binding:typed-variable-binding ...) body:cond-form)
             #:with (var ...) #'(var-binding.var ...)
             #:with (type ...) #'(var-binding.type ...)
             #:with (binding ...) #'(var-binding.binding ...)
             #:with test #'body.test
             #:with (then ...) #'(body.then ...)
             #:with (then.type ...) #'(body.then.type ...)
             #:with (then.binding ...) #'(body.then.binding ...)
             #:with color #'body.color
             #:with (colors ...) #'(body.colors ...)
             #:with (else ...) #'(body.else ...)
             #:with (else.type ...) #'(body.else.type ...)
             #:with (else.binding ...) #'(body.else.binding ...)
             #:with iterate #'body.iterate))

  (define-syntax-class iterator-definition
    #:description "iterator definition"
    (pattern (_ header:typed-function-header body:loop-form)
             #:with name #'header.name
             #:with (arg ...) #'(header.arg ...)
             #:with (arg.type ...) #'(header.type ...)
             #:with (var ...) #'(body.var ...)
             #:with (var.type ...) #'(body.type ...)
             #:with (var.binding ...) #'(body.binding ...)
             #:with test #'body.test
             #:with (then ...) #'(body.then ...)
             #:with (then.type ...) #'(body.then.type ...)
             #:with (then.binding ...) #'(body.then.binding ...)
             #:with color #'body.color
             #:with (colors ...) #'(body.colors ...)
             #:with (else ...) #'(body.else ...)
             #:with (else.type ...) #'(body.else.type ...)
             #:with (else.binding ...) #'(body.else.binding ...)
             #:with iterate #'body.iterate)))

(define-syntax (define-iterator stx)
  (syntax-parse stx
    [i:iterator-definition
     (with-syntax* ([module-body
                     #'(iterator:racket
                        typed/racket
                        (provide (struct-out color) (rename-out [i.name iterator]))
                        (struct color ([red : Integer]
                                       [green : Integer]
                                       [blue : Integer]
                                       [alpha : Integer])
                          #:prefab)
                        (define (i.name [i.arg : i.arg.type] ...) : color
                          (let loop : color ([i.var : i.var.type i.var.binding] ...)
                               (cond [i.test
                                      (define i.then : i.then.type i.then.binding)
                                      ...
                                      (color
                                       (floor
                                        (inexact->exact (* 255.0 i.colors))) ...)]
                                     [else
                                      (define i.else : i.else.type i.else.binding)
                                      ...
                                      i.iterate]))))]
                    [stripped-context
                     #`#,(syntax-local-introduce (strip-context #'module-body))])
       #`(begin
           (module . stripped-context)))]))

#;(define-syntax (compile-glsl-program stx)
  (syntax-parse stx
    [(_ f:fragment-shader)
     #`#,(~a* `(,@(compile-varying-defs #'(f.arg.type ...) #'(f.arg ...))
                ,(compile-main #'(f.pre-loop-var ...)
                               #'(f.pre-loop-var.type ...)
                               #'(f.pre-loop-var.binding ...)
                               #'(f.end-loop ...)
                               #'(f.loop-var ...)
                               #'(f.loop-var.type ...)
                               #'(f.loop-var.binding ...)
                               #'(f.assignment ...)
                               #'(f.assignment.binding ...)
                               #'(f.colors ...))))]))

(define-syntax (compile-glsl-program stx)
  (syntax-parse stx
    [(_ f:iterator-definition)
     #`#,(~a* `(,@(compile-varying-defs #'(f.arg.type ...) #'(f.arg ...))
                ,(compile-main #'(f.var ...)
                               #'(f.var.type ...)
                               #'(f.var.binding ...)
                               #'f.test
                               #'(f.else ...)
                               #'(f.else.type ...)
                               #'(f.else.binding ...)
                               #'(f.var ...)
                               #`#,(rest (syntax->list #'f.iterate))
                               #'(f.then ...)
                               #'(f.then.type ...)
                               #'(f.then.binding ...)
                               #'(f.colors ...))))]))

(require racket/gui opengl opengl/util racket/flonum)

(define (resize w h)
  (glViewport 0 0 w h))

(define vertex-shader
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
 xpos = real + zoom * (gl_Vertex.x * w - w / 2);
 ypos = imag + zoom * (gl_Vertex.y * h - h / 2);

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
                         (imag 1f (fl- i))
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
             (send this refresh)]))

    (define/override (on-paint)
      (with-gl-context
          (λ()
            (unless the-program
              (set! the-program
                    (create-program
                      (load-shader (open-input-string fragment-shader)
                                   GL_FRAGMENT_SHADER)
                      (load-shader (open-input-string vertex-shader)
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

(define frame (new frame% [label "OpenGL test"]
                   [width 600]
                   [height 600]))

(define canvas (new my-canvas% [parent frame]))

(send canvas focus)

(send frame show #t)

(define fragment-shader
  (compile-glsl-program
   (define-iterator (mandelbrot [xpos : Float] [ypos : Float])
     (let loop ([z_real : Float 0.0]
                [z_imaginary : Float 0.0]
                [iterations : Float 0.0]
                [sq : Float 0.0])
       (cond [(or (> iterations 1.0)
                  (> sq 4.0))
              (define red : Float iterations)
              (define green : Float iterations)
              (define blue : Float iterations)
              (define alpha : Float 1.0)
              (color red green blue alpha)]
             [else
              (define z_real_square : Float
                (* z_real z_real))
              (define z_imaginary_square : Float
                (* z_imaginary z_imaginary))
              (define z_real_new : Float
                (+ xpos (- z_real_square z_imaginary_square)))
              (define z_imaginary_new : Float
                (+ (* 2.0 (* z_real z_imaginary)) ypos))
              (define iterations_new : Float
                (+ iterations 0.005))
              (define sq_new : Float
                (+ z_real_square z_imaginary_square))
              (loop z_real_new
                    z_imaginary_new
                    iterations_new
                    sq_new)])))))

(display fragment-shader)

#;(([float xpos]
    [float ypos])
   (([float z_real 0.0]
     [float z_imaginary 0.0]
     [float iterations 0.0]
     [float a xpos]
     [float bi ypos]
     [float sq 0.0])
    ((> iterations 1.0)
     (> sq 4.0))
    ([float z_real_square (* z_real z_real)]
     [float z_imaginary_square (* z_imaginary z_imaginary)]
     [float z_real_new (+ (- z_real_square z_imaginary_square) a)]
     [float z_imaginary_new (+ (* 2.0 (* z_real z_imaginary)) bi)])
    ([z_real z_real_new]
     [z_imaginary z_imaginary_new]
     [iterations (+ iterations 0.005)]
     [sq (+ z_real_square z_imaginary_square)])
    (iterations iterations iterations 1.0)))
