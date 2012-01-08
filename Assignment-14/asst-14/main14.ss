
(eval-when (compile load eval)
  (optimize-level 2)
  (case-sensitive #t)
)

(load "match.ss")
(load "helpers.ss")
(load "fmts.pretty")     ; inform pretty-print about new forms
(load "driver.ss")

(load "a14.ss")
(load "a14-wrapper.ss")   ; defines syntactic forms and procedures
                         ; needed to output of each pass
(compiler-passes '(
  verify-scheme
	convert-complex-datum
	uncover-assigned
	purify-letrec
	convert-assignments
))

(tracer #t)
(load "tests14.ss")

(test-one (list-ref tests 160))

;(test-one '(let ([x.3 '10] [y.1 '11] [z.2 '12])
;    (begin
;      (set! x.3 (+ x.3 y.1))
;      (set! z.2 (* y.1 '2))
;      (cons y.1 z.2))))




