
(eval-when (compile load eval)
  (optimize-level 2)
  (case-sensitive #t)
)

(load "match.ss")
(load "helpers.ss")
(load "fmts.pretty")     ; inform pretty-print about new forms
(load "driver.ss")

(load "a13.ss")
(load "a13-wrapper.ss")   ; defines syntactic forms and procedures
                         ; needed to output of each pass
(compiler-passes '(
  verify-scheme
  optimize-direct-call
	remove-anonymous-lambda
	sanitize-binding-forms
	uncover-free
	convert-closures
	optimize-known-call
))

(tracer #t)
(load "tests13.ss")

;(test-one (list-ref tests 108))

(test-one '(letrec ([add.0 (lambda (n.2)
										(lambda (n.3)
											(+ n.2 n.3)))]
				 [map.1 (lambda (fn.4 ls.5) 
									(if (null? ls.5)
										'()
										 (cons (fn.4 (car ls.5)) (map.1 fn.4 (cdr ls.5)))))]
				 [map.9 (lambda (fn.10 fnls.11 ls.12)
										(if (null? ls.12) 
											'()
										(cons (fn.10 (car fnls.11) (car ls.12)) (map.9 fn.10 (cdr fnls.11) (cdr ls.12)))))])
  (let ([ls.6 (cons '1 (cons '2 (cons '3 '())))])
			(map.9 (lambda (fn.7 elem.8) 
								(fn.7 elem.8)) (map.1 add.0 ls.6) ls.6))))



