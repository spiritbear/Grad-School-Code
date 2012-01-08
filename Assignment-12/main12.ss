
(eval-when (compile load eval)
  (optimize-level 2)
  (case-sensitive #t)
)

(load "match.ss")
(load "helpers.ss")
(load "fmts.pretty")     ; inform pretty-print about new forms
(load "driver.ss")

(load "a12.ss")
(load "a12-wrapper.ss")   ; defines syntactic forms and procedures
                         ; needed to output of each pass
(compiler-passes '(
  verify-scheme
	uncover-free
	convert-closures
	introduce-procedure-primitives
	lift-letrec
	normalize-context
	specify-representation
))

(tracer #t)
(load "tests12.ss")
(test-one (list-ref tests 12))

;(test-one '(let ([n.1 '7])
;  (letrec ([f.2 (lambda (x.3)
;                  (if x.3 '#f '#t))]
;           [g.4 (lambda ()
;                  (letrec ([h.5 (lambda (y.6)
;                                  (cons (f.2 (< y.6 n.1))
;                                        (f.2 (< n.1 y.6))))])
;                    h.5))])
;    ((g.4) '7))))



