(letrec ([odd$6 (lambda (n.5) (if (= n.5 '0) '#f (even$7 (- n.5 '1))))]
				 [even$7 (lambda (n.8) (if (= n.8 '0) '#t (odd$6 (- n.8 '1))))]
				 [filter$2 (lambda (f.10 ls.11) 
						(if (null? ls.11)
							'()
						 	 (if (eq? (f.10 (car ls.11)) '#t)
										(cons (car ls.11) (filter$2 f.10 (cdr ls.11)))
										(filter$2 f.10 (cdr ls.11)))))])
	(let ([ls.1 (cons '6 (cons '5 (cons '4 (cons '3 (cons '2 (cons '1 '()))))))])
		(filter$2 odd$6 ls.1)))