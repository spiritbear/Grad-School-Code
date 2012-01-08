(letrec ([sum$0 (lambda (n.1) (locals () (sum$1 n.1 0)))]
				 [sum$1
						(lambda (n.1 s.2)
							(locals ()
								(if (= n.1 0)
									s.2
									(sum$1 (- n.1 1) (+ s.2 n.1)))))])
(locals ()
	(sum$0 20)))