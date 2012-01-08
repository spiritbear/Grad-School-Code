(eval-when (compile load eval)
  (optimize-level 2)
  (case-sensitive #t)
)

(load "match.ss")
(load "helpers.ss")
(load "fmts.pretty")     ; inform pretty-print about new forms
(load "driver.ss")

(load "a5.ss")
(load "a5-wrapper.ss")   ; defines syntactic forms and procedures
                         ; needed to output of each pass


(compiler-passes '(
  verify-scheme
	uncover-frame-conflict
	introduce-allocation-forms
	(iterate
	 	select-instructions
	  uncover-register-conflict
    assign-registers
   (break when everybody-home?)
    assign-frame
    finalize-frame-locations)
		discard-call-live
		finalize-locations
		expose-frame-var
		expose-basic-blocks
 	  flatten-program
	  generate-x86-64
))

(tracer #t)
(load "tests5.ss")

(define xyz
	(lambda (x)
		(map (lambda (x) 
						(if (frame-var? x) (frame-var->index x) '0)) '(fv1 fv0 fv2 rax))))
						
(test-one 
	'(letrec ()
     (locals (x.1 y.2 z.3 t.4 t.5)
       (begin
         (set! rax 5)
         (set! x.1 10)
         (set! y.2 20)
         (set! z.3 30)
         (if (if (begin 
                   (set! t.4 (+ x.1 y.2))
                   (set! t.5 (+ rax z.3)) 
                   (> t.4 t.5))
                 (< z.3 x.1)
                 (> z.3 y.2))
             (set! rax (+ t.4 t.5))
             (set! rax (- t.5 t.4)))
         (set! z.3 (+ x.1 y.2))
         (set! rax (* rax z.3))
         (r15 rax rbp rbx rcx rdx rdi rsi r9 r10 r11 r12 r13 r14)))))


;(test-one (list-ref tests 14))