 (eval-when (compile load eval)
   (optimize-level 2)
   (case-sensitive #t)
 )

 (load "match.ss")
 (load "helpers.ss")
 (load "fmts.pretty")     ; inform pretty-print about new forms
 (load "driver.ss")

 (load "a7.ss")
 (load "a7-wrapper.ss")   ; defines syntactic forms and procedures
                          ; needed to output of each pass
 (compiler-passes '(
   verify-scheme
		remove-complex-opera*
		flatten-set!
		impose-calling-conventions
		uncover-frame-conflict
		pre-assign-frame
		assign-new-frame
		(iterate 
			finalize-frame-locations
			select-instructions
			uncover-register-conflict
			assign-registers
			(break when everybody-home?)
			assign-frame)
		discard-call-live
		finalize-locations
		expose-frame-var
		expose-basic-blocks
		flatten-program
   	generate-x86-64
 ))

(load "tests7.ss")
(tracer #t)

(test-one '(letrec ([ack$1 (lambda (m.1 n.2)
                      (locals (t.3)
                        (if (= m.1 0) (+ n.2 1)
													(if (= n.2 0) 
																(ack$1 (- m.1 1) 1)
																(begin
																	(set! t.3 (ack$1 m.1 (- n.2 1)))
																	(ack$1 (- m.1 1) t.3))))))])
   (locals () (ack$1 2 4))))