#lang racket/base

(require syntax/parse)

(provide (all-defined-out))

(define-syntax-class type
  #:datum-literals (Float Integer)
  (pattern Float)
  (pattern Integer))

(define-syntax-class typed-variable
  #:attributes (name type)
  #:datum-literals (:)
  (pattern [name:id : type:type]))

(define-syntax-class header
  #:attributes (name (argument 1))
  (pattern (name:id argument:typed-variable ...)))

(define-syntax-class arithmetic
  #:attributes (expression)
  #:datum-literals (+ - * /)
  (pattern var:id #:with expression #'var)
  (pattern n:number #:with expression #'n)
  (pattern (+ a:arithmetic b:arithmetic) #:with expression #'(+ a b))
  (pattern (- a:arithmetic b:arithmetic) #:with expression #'(- a b))
  (pattern (* a:arithmetic b:arithmetic) #:with expression #'(* a b))
  (pattern (/ a:arithmetic b:arithmetic) #:with expression #'(/ a b)))

(define-syntax-class conditional
  #:attributes (expression)
  #:datum-literals (or and not < > <= >= =)
  (pattern b:boolean #:with expression #'b)
  (pattern (or a:conditional b:conditional) #:with expression #'(or a b))
  (pattern (and a:conditional b:conditional) #:with expression #'(and a b))
  (pattern (not a:conditional) #:with expression #'(not a))
  (pattern (< a:arithmetic b:arithmetic) #:with expression #'(< a b))
  (pattern (> a:arithmetic b:arithmetic) #:with expression #'(> a b))
  (pattern (<= a:arithmetic b:arithmetic) #:with expression #'(<= a b))
  (pattern (>= a:arithmetic b:arithmetic) #:with expression #'(>= a b))
  (pattern (= a:arithmetic b:arithmetic) #:with expression #'(= a b)))

(define-syntax-class cond-form
  #:attributes ((conditional 1)
                (body 2)
                (return 1)
                (else-body 1)
                (else-return 0))
  #:datum-literals (cond else)
  (pattern (cond [conditional:conditional body:toplevel-form ... return:form] ...
                 [else else-body:toplevel-form ... else-return:form])))

(define-syntax-class typed-variable-binding
  #:attributes (name type expression)
  #:datum-literals (:)
  (pattern [name:id : type:type binding:arithmetic]
           #:with expression #'binding.expression))

(define-syntax-class definition
  #:attributes (name type binding)
  #:datum-literals (define :)
  (pattern (define name:id : type:type binding:form)))

(define-syntax-class loop
  #:attributes (name (argument 1) (body 1))
  #:datum-literals (let)
  (pattern (let name:id (argument:typed-variable-binding ...)
             body:toplevel-form ...
             return:form)))

(define-syntax-class application
  #:attributes (function (argument 1))
  (pattern (function:id argument:form ...)))

(define-syntax-class form
  #:attributes (loop cond-form application expression)
  (pattern (~or* loop:loop cond-form:cond-form application:application expression:arithmetic)))

(define-syntax-class toplevel-form
  #:attributes (definition form)
  (pattern (~or* definition:definition form:form)))

(define-syntax-class iterator
  #:attributes (header (body 1) return)
  (pattern (_ header:header body:toplevel-form ... return:form)))
