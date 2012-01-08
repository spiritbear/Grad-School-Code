
(eval-when (compile load eval)
  (optimize-level 2)
  (case-sensitive #t)
)

(load "match.ss")
(load "helpers.ss")
(load "fmts.pretty")     ; inform pretty-print about new forms
(load "driver.ss")

(load "a11.ss")
(load "a10-previous.ss")
(load "a9-previous.ss")
(load "a8-previous.ss")
;;(load "../asst-10/a10-wrapper.ss")
;;(load "../asst-9/a9-wrapper.ss")
;;(load "a8-wrapper.ss")
(load "a11-wrapper.ss")   ; defines syntactic forms and procedures
                         ; needed to output of each pass
(compiler-passes '(
  verify-scheme
	lift-letrec
	normalize-context
	specify-representation
			uncover-locals
							remove-let
							verify-uil
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

(tracer '(expose-basic-blocks))
(load "tests11.ss")
(test-one (list-ref tests 293))
;(test-one '(letrec ([isort$2 (lambda (ls.3)
;	(if (null? ls.3) '()
;		(insert$5 (car ls.3) (isort$2 (cdr ls.3)))))] 
;				 [insert$5 (lambda (val.6 ls.7)
;						(letrec ([add$9 (lambda (n.10) (+ n.10 '1))])
;							(if (null? ls.7)
;										(cons (add$9 val.6) '())
;											(if (< val.6 (- (car ls.7) '1))
;												(cons (add$9 val.6) ls.7)
;												(cons (car ls.7) (insert$5 val.6 (cdr ls.7)))))))])
;(let ([ls.1 (cons '6 (cons '5 (cons '4 (cons '3 (cons '2 (cons '1 '()))))))])
;		(isort$2 ls.1))))


