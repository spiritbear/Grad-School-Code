(eval-when (compile load eval)
   (optimize-level 2)
   (case-sensitive #t)
)

(load "match.ss")
(load "helpers.ss")
(load "fmts.pretty")     ; inform pretty-print about new forms
(load "driver.ss")

(load "a6.ss")
(load "a6-wrapper.ss")   ; defines syntactic forms and procedures
                         ; needed to output of each pass
(compiler-passes '(
  verify-scheme
  remove-complex-opera*
	flatten-set!
	impose-calling-conventions
))

(load "tests6.ss")
(tracer #t)

;(test-one '(letrec ([fact$0 (lambda (n.1)
;                   (locals ()
;                     (fact$1 n.1 1)))]
;         [fact$1 (lambda (n.1 a.2)
;                   (locals ()
;                     (if (= n.1 0)
;                         a.2
;                         (fact$1 (- n.1 1) (* n.1 a.2)))))])
;  (locals () (fact$0 10))))

(test-all)

;(test-one '(letrec ([dot$0 (lambda (a.1 a.2 a.3 a.4 b.5 b.6 b.7 b.8)
;                    (locals ()
;                      (+ (* a.1 b.5) 
;                         (+ (* a.2 b.6) 
;                            (+ (* a.3 b.7) (* a.4 b.8))))))])
;    (locals () (dot$0 2 4 6 8 1 3 5 7))))