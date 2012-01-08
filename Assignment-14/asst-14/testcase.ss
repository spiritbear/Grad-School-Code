(letrec ([add.0 (lambda (n.2)
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
  (let ([ls.6 '(1 2 3 4 5 6)])
			(map.9 (lambda (fn.7 elem.8) 
								(fn.7 elem.8)) (map.1 add.0 ls.6) ls.6)))
								
(let ([t.1 (lambda () '(1 2))])
	(t.1))
	
	(letrec ((f (if (eq? 2 3) (cons 1 (g 2)) (cons 2 (g 2))))
	         (g (lambda (y) ((car f) ((car f) y)))))
	  (+ ((car f) (cdr f)) (g 1)))