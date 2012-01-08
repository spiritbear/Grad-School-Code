
(eval-when (compile load eval)
  (optimize-level 2)
  (case-sensitive #t)
)

(load "match.ss")
(load "helpers.ss")
(load "fmts.pretty")     ; inform pretty-print about new forms
(load "driver.ss")

(load "a15.ss")
(load "a15-wrapper.ss")   ; defines syntactic forms and procedures
                         ; needed to output of each pass
;(compiler-passes '(
;  parse-scheme
;	convert-complex-datum
;	uncover-assigned
;	purify-letrec
;	convert-assignments
;	remove-anonymous-lambda
;	sanitize-binding-forms
;  uncover-free
;	convert-closures
;	introduce-procedure-primitives
;	lift-letrec
;	normalize-context
;	specify-representation
;	uncover-locals
;	remove-let
;	verify-uil
;	remove-complex-opera*
;))


(load "tests15.ss")

(tracer '(parse-scheme))

(test-one '(let ([f (lambda (y)
           					(lambda (p)
             					(cons y (p y))))])
  						(letrec ([g (lambda (n)
                (if (= n 0)
                    '()
                    ((f (- n 1)) g)))])
    (g 6))))






