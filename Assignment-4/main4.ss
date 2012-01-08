 ;Basic Usage:
 ;
 ;To use the driver for Assignment 1, create a file containing:

 (eval-when (compile load eval)
   (optimize-level 2)
   (case-sensitive #t)
 )

 (load "match.ss")
 (load "helpers.ss")
 (load "fmts.pretty")     ; inform pretty-print about new forms
 (load "driver.ss")

 (load "a4.ss")
 (load "a4-wrapper.ss")   ; defines syntactic forms and procedures
                          ; needed to output of each pass
 (compiler-passes '(
   verify-scheme
   uncover-register-conflict
   assign-registers
	 discard-call-live
	 finalize-locations
	 expose-frame-var
   expose-basic-blocks
	 flatten-program
	 generate-x86-64
 ))


(load "tests4.ss")
(tracer #t)
(test-one '(letrec ([if-test$1 (lambda ()
                       (locals (x.5)
                         (begin
                           (if (begin (set! x.5 5) (= x.5 5))
                               (set! x.5 (+ x.5 10))
                               (set! x.5 (- x.5 10)))
                           (set! x.5 (* x.5 10))
                           (set! rax x.5)
                           (r15 rax))))])
   (locals () (if-test$1 r15))))
