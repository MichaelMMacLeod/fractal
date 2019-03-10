#lang racket

(require syntax/parse
         (for-syntax racket/syntax
                     syntax/parse
                     syntax/to-string
                     syntax/strip-context))

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
             #:with (then ...) #'(then-body ...)
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
             #:with body #'(else-body ...)
             #:with iterate #'loop-iterate-clause))

  (define-syntax-class cond-form
    #:description "cond clause"
    #:datum-literals (cond)
    (pattern (cond test-clause:test-clause ...+ else-clause:else-clause)
             #:with (test ...) #'(test-clause.test ...)
             #:with ((then ...) ...) #'((test-clause.then ...) ...)
             #:with (color ...) #'(test-clause.color ...)
             #:with ((colors ...) ...) #'((test-clause.colors ...) ...)
             #:with (else ...) #'else-clause.body
             #:with iterate #'else-clause.iterate))

  (define-syntax-class loop-form
    #:description "loop form"
    #:datum-literals (let loop)
    (pattern (let loop (var-binding:typed-variable-binding ...) body:cond-form)
             #:with (var ...) #'(var-binding.var ...)
             #:with (type ...) #'(var-binding.type ...)
             #:with (binding ...) #'(var-binding.binding ...)
             #:with (test ...) #'(body.test ...)
             #:with ((then ...) ...) #'((body.then ...) ...)
             #:with (color ...) #'(body.color ...)
             #:with ((colors ...) ...) #'((body.colors ...) ...)
             #:with (else ...) #'(body.else ...)
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
             #:with (test ...) #'(body.test ...)
             #:with ((then ...) ...) #'((body.then ...) ...)
             #:with (color ...) #'(body.color ...)
             #:with ((colors ...) ...) #'((body.colors ...) ...)
             #:with (else ...) #'(body.else ...)
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
                                      i.then ...
                                      (color
                                       (floor
                                        (inexact->exact (* 255.0 i.colors))) ...)]
                                     ...
                                     [else i.else ...
                                           i.iterate]))))]
                    [stripped-context
                     #`#,(syntax-local-introduce (strip-context #'module-body))])
       #'(begin (module . stripped-context)))]))

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
           (loop (+ xpos (- z_real_square z_imaginary_square))
                 (+ (* 2.0 z_real z_imaginary) ypos)
                 (+ iterations 0.005)
                 (+ z_real_square z_imaginary_square))])))
