(eval-when (compile load eval)
  (optimize-level 2)
  (case-sensitive #t)
)

(load "match.ss")
(load "helpers.ss")
(load "fmts.pretty")     ; inform pretty-print about new forms
(load "driver.ss")

(load "a9.ss")
(load "a9-wrapper.ss")   ; defines syntactic forms and procedures
                         ; needed to output of each pass
(compiler-passes '(
  verify-scheme
	uncover-locals
	remove-let
  ;generate-x86-64
))

(load "tests9.ss")
(tracer #t)
(test-one (list-ref tests 13))