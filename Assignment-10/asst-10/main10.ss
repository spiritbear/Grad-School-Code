(eval-when (compile load eval)
  (optimize-level 2)
  (case-sensitive #t)
)


(load "match.ss")
(load "helpers.ss")
(load "fmts.pretty")     ; inform pretty-print about new forms
(load "driver.ss")

(load "a10.ss")
(load "a10-wrapper.ss")   ; defines syntactic forms and procedures
                         ; needed to output of each pass
(compiler-passes '(
  verify-scheme
  specify-representation
	;uncover-locals
))

(tracer #t)
(load "tests10.ss")

(test-one (list-ref tests 7))		

