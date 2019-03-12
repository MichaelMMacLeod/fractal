#lang racket

(require syntax/parse
         (for-syntax racket
                     racket/syntax
                     syntax/parse
                     syntax/to-string
                     syntax/strip-context))

(provide define-iterator)

(begin-for-syntax
  (define (indent n strings #:spaces [spaces 2])
    (for/list ([s (in-list strings)])
      (string-append (make-string (* spaces n) #\Space) s)))

  (define (type->glsl-type type)
    (match type
      ['Float 'float]
      ['Integer 'int]))

  (define (compile-in-defs types args)
    (for/list ([type (in-list (syntax->list types))]
               [arg (in-list (syntax->list args))])
      (format "in ~a ~a;"
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
                                 (compile-arithmetic-expression a)
                                 (compile-arithmetic-expression b))]
      [`(> ,a ,b)
       (compile-infix-expression ">"
                                 (compile-arithmetic-expression a)
                                 (compile-arithmetic-expression b))]
      [`(<= ,a ,b)
       (compile-infix-expression "<="
                                 (compile-arithmetic-expression a)
                                 (compile-arithmetic-expression b))]
      [`(>= ,a ,b)
       (compile-infix-expression ">="
                                 (compile-arithmetic-expression a)
                                 (compile-arithmetic-expression b))]
      [`(= ,a ,b)
       (compile-infix-expression "="
                                 (compile-arithmetic-expression a)
                                 (compile-arithmetic-expression b))]
      [x (~a x)]))

  (define (compile-loop-condition end-loop)
    (format
     "while (!~a) {"
     (format "~a" (compile-conditional-expression
                   (syntax->datum end-loop)))))

  (define (genstring already-defined [base "g_"])
    (define g (gensym base))
    (cond [(member g already-defined)
           (genstring already-defined base)]
          [else g]))

  (define (compile-assignments assignments types assignment-bindings already-defined)
    (define vars (syntax->list assignments))
    (define genstrings
      (for/list ([var (in-list vars)])
        (genstring already-defined (syntax-e var))))
    (append
     (compile-binding-declarations (datum->syntax assignments genstrings assignments)
                                   types
                                   assignment-bindings)
     (for/list ([var (in-list vars)]
                [genstring (in-list genstrings)])
       (format "~a = ~a;" (syntax-e var) genstring))))

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
    `("void main(void) {"
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
                ,@(compile-assignments assignments
                                       pre-loop-var-types
                                       assignment-bindings
                                       (append (syntax->datum pre-loop-vars)
                                               (syntax->datum loop-vars)
                                               (syntax->datum assignments)
                                               (syntax->datum post-vars)))))))
      ,@(indent 1 '("}"))
      ,@(indent
         1
         `(,@(compile-binding-declarations post-vars
                                           post-var-types
                                           post-var-bindings)))
      ,@(indent 1 (list (compile-colors colors)))
      "}")))

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

(define-syntax (compile-glsl-program stx)
  (syntax-parse stx
    [f:iterator-definition
     #`#,(~a* `(,@(compile-in-defs #'(f.arg.type ...) #'(f.arg ...))
                ,@(compile-main #'(f.var ...)
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
#version 420

uniform float real;
uniform float imag;
uniform float w;
uniform float h;
uniform float zoom;

in vec2 vertex_position;

out float xpos;
out float ypos;

void main(void)
{
 // vertex_position.x,y :: [-2.0 .. 2.0]
 //float vert_x = (vertex_position.x + 2.0) / 4.0;
 //float vert_y = (vertex_position.y + 2.0) / 4.0;
 float vert_x = vertex_position.x;
 float vert_y = vertex_position.y;

 xpos = real + zoom * vert_x * w;
 ypos = imag + zoom * vert_y * h;

 gl_Position = vec4(vert_x, vert_y, 1.0, 1.0);
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

(require ffi/vector (only-in ffi/unsafe _cpointer/null))

(define (draw-opengl prg r i w h z)
  (glClearColor 0.0 0.0 0.0 0.0)
  (glClear GL_COLOR_BUFFER_BIT)

  (glSetUniformLocations prg
                         (real 1f r)
                         (imag 1f (fl- i))
                         (zoom 1f z)
                         (w 1f w)
                         (h 1f h))
  (let ([buffer (u32vector-ref (glGenBuffers 1) 0)]
        [positionData (f32vector -1.0 -1.0 0.0
                                 1.0 -1.0 0.0
                                 -1.0 1.0 0.0
                                 -1.0 1.0 0.0
                                 1.0 -1.0 0.0
                                 1.0 1.0 0.0)])
    (glBindBuffer GL_ARRAY_BUFFER buffer)
    (glBufferData GL_ARRAY_BUFFER
                  (* 18 (gl-type-sizeof GL_FLOAT))
                  (gl-vector->cpointer positionData)
                  GL_STATIC_DRAW)
    (let ([vertex-array (glGenVertexArrays 1)])
      (glBindVertexArray 1)
      (glEnableVertexAttribArray 0)
      (glBindBuffer GL_ARRAY_BUFFER buffer)
      (glVertexAttribPointer 0 3 GL_FLOAT #f 0 #f)
      (glBindVertexArray 1)
      (glDrawArrays GL_TRIANGLES 0 6))))

(define my-canvas%
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)
    (define the-program #f)

    (define real 0.0)
    (define imag 0.0)
    (define zoom 0.005)

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
    (define gl-config (new gl-config%))
    (send gl-config set-legacy? #f)
    (super-new [gl-config gl-config]
               [style '(gl no-autoclear)])
    #;(super-instantiate () (style '(gl no-autoclear)))))

(define frame (new frame% [label "OpenGL test"]
                   [width 600]
                   [height 600]))

(define canvas (new my-canvas% [parent frame]))

(send canvas focus)

(send frame show #t)

(define fragment-shader
  (compile-glsl-program (julia [xpos : Float] [ypos : Float])
    (let loop ([z_real : Float xpos]
               [z_imaginary : Float ypos]
               [iterations : Float 0.0])
         (cond [(or (> (+ (* z_real z_real)
                          (* z_imaginary z_imaginary))
                       4.0)
                    (>= iterations 1.0))
                (color iterations iterations iterations 1.0)]
               [else
                (define z_real_square : Float (* z_real z_real))
                (define z_imaginary_square : Float (* z_imaginary z_imaginary))
                (define c_real : Float 0.35)
                (define c_imaginary : Float 0.35)
                (loop (+ (- z_real_square z_imaginary_square) c_real)
                      (+ (* 2.0 (* z_real z_imaginary)) c_imaginary)
                      (+ iterations 0.05))]))))

#;(define fragment-shader
  (compile-glsl-program (mandelbrot [xpos : Float] [ypos : Float])
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
              (loop (+ xpos (- z_real_square z_imaginary_square))
                    (+ (* 2.0 (* z_real z_imaginary)) ypos)
                    (+ iterations 0.005)
                    (+ z_real_square z_imaginary_square))]))))

