#lang at-exp racket/base

(require (for-syntax "iterator.rkt"
                     racket/base
                     syntax/parse
                     syntax/strip-context))

(provide define-iterator)

(define-for-syntax (compile-iterator/typed stx)
  (syntax-parse stx
    [(_ (name:id argument:typed-variable ...) body:toplevel-form ... return:form)
     #`(module
           #,@(syntax-local-introduce
               (strip-context
                #'(iterator:typed typed/racket/base
                                  (provide iterator)
                                  (struct color ([r : Float]
                                                 [g : Float]
                                                 [b : Float]
                                                 [a : Float])
                                    #:prefab)
                                  (define (iterator argument ...) : color
                                    body ...
                                    return)))))]))

(define-for-syntax (compile-function-application stx)
  (syntax-parse stx
    ))

(define-for-syntax (compile-iterator/opengl stx)
  #`(module
        #,@(syntax-local-introduce
            (strip-context
             #'(iterator:opengl racket/base
                                (provide iterator)
                                (require racket/format)
                                #;(define iterator #,(compile-toplevel stx))
                                (define iterator
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
                                      }))))))

(define-syntax (define-iterator stx)
  (syntax-parse stx
    [i:iterator
     #`(begin
         #,(compile-iterator/typed stx)
         #,(compile-iterator/opengl stx))]))
