(letrec ([isort$2 (lambda (ls.3)
	(if (null? ls.3) '()
		(insert$5 (car ls.3) (isort$2 (cdr ls.3)))))] 
				 [insert$5 (lambda (val.6 ls.7)
						(letrec ([add$9 (lambda (n.10) (+ n.10 '1))])
							(if (null? ls.7)
										(cons (add$9 val.6) '())
											(if (< val.6 (- (car ls.7) '1))
												(cons (add$9 val.6) ls.7)
												(cons (car ls.7) (insert$5 val.6 (cdr ls.7)))))))])
(let ([ls.1 (cons '6 (cons '5 (cons '4 (cons '3 (cons '2 (cons '1 '()))))))])
		(isort$2 ls.1)))
		
		
		(define build-assoc-list
			(lambda (label* body* last*)
				(remq '() (map (lambda (label body last)
								(if (and (null? body) (label? last))
									 (let [(symbol (walk last assoc-list '#f))]
											(if (eq? symbol '#f)
													(set! assoc-list (cons (cons label last) assoc-list))
													(set! assoc-list (cons (cons label symbol) assoc-list)))
													'())
									`(,label (lambda () (,@body ,last))))) label* body* last*))))
							

(let [(symbol (walk last assoc-list '#f))]
	(if (eq? symbol '#f)
		(set! assoc-list (cons (cons label last) assoc-list))
		(set! assoc-list (cons '() assoc-list))))