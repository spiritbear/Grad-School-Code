;;;This is a test case for Assignment-8

(letrec ([get$0 (lambda (x.1 ls.2)
										(locals (size.4 ls.3)
											(begin
												(set! size.4 (mref ls.2 0))
												(if (> x.1 size.4) 
												'-1
												(mref ls.2 (+ ls.2 (* 8 x.1)))))))])
	(locals (ls.1)
		(begin
			(set! ls.1 (alloc 48))
			(mset! ls.1 0 5)
			(mset! ls.1 8 9)
			(mset! ls.1 16 2)
			(mset! ls.1 24 7)
			(mset! ls.1 32 8)
			(mset! ls.1 40 3)
			(get$0 4 ls.1))))


;;; Wether x is a member of ls : if true return 1 else return 0
(letrec ([member$0 (lambda (x.1 ls.2)
										(locals (size.4 ls.3)
											(begin
												(set! size.4 (mref ls.2 0))
												(if (> x.1 size.4) 
												'0
												(if (= x.1 (mref ls.2 16)) 
													'1
													(begin
														(set! ls.3 (alloc (* 8 (- size.4 1))))
														(mset! ls.3 0 (- size.4 1))
														(mset! ls.3 8 (+ ls.2 16))
														(member$0 x.1 ls.3)))))))])
	(locals (ls.1)
		(begin
			(set! ls.1 (alloc 48))
			(mset! ls.1 0 5)
			(mset! ls.1 8 9)
			(mset! ls.1 16 2)
			(mset! ls.1 24 7)
			(mset! ls.1 32 8)
			(mset! ls.1 40 3)
			(member$0 4 ls.1))))

;;;insertafter takes an index,a value and a list
;;; inserts value after that index and returns the list

(letrec ([insertafter$0 
						(lambda (pos.2 val.3 ls.1 output.4 count.5)
							(locals (size.6)
								(begin
									(set! size.6 (mref ls.1 0))
									(if (> pos.2 size.6)
										(begin
											(mset! output.4 (+ ls.1 8))
											(mset! output.4 (* size.4 8) val.3)
											output.4)
										(if (= pos.2 0)
												(begin
													(mset! output.4 count.5 val.3)
													(mset! output.4 (+ count.5 8) (mref ls.1 (+ count.5 8)))
													output.4)
												(begin
													(mset! output.4 count.5 (mref ls.1 count.5))
													(insertafter$0 (- pos.2 1) val.3 ls.1 output.4 (+ count.5 8))))))))]
	(locals (ls.1 pos.2 val.3 output.4 count.5)
		(begin
			(set! pos.2 0)
			(set! val.3 19)
			(set! ls.1 (alloc 48))
			(mset! output.4 (alloc 48)) ;;Alloc one more location for output
			(set! count.5 0)
			(mset! ls.1 0 5)
			(mset! ls.1 8 9)
			(mset! ls.1 16 2)
			(mset! ls.1 24 7)
			(mset! ls.1 32 8)
			(mset! ls.1 40 3)
			(insertafter$0 pos.2 val.3 ls.1 output.4 count.5))))
			
				[(set! ,lhs (mref ,x ,y))
					(if (ur? lhs)
						(cond
								[(or (and (ur? x) (int32? y)) (and (ur? x) (ur? y)) (and (int32? x) (ur? y)) (and (integer? x) (integer? y))) ef]
								[(or (and (ur? x) (frame-var? y)) (and (integer? x) (frame-var? y))) 
									(let ((u2 (new-u)))
										`(begin (set! ,u2 ,y) (set! ,lhs (mref ,x ,u2))))]
								[(or (and (ur? y) (frame-var? x)) (and (frame-var? x) (integer? y))) 
									(let ((u1 (new-u)))
										`(begin (set! ,u1 ,x) (set! ,lhs (mref ,u1 ,y))))]
								[else (printf "Lhs ur but rhs not handled ~%")])
						(let* ((u (new-u)) (effect (Effect `(set! ,u (mref ,x ,y)))))
							(make-begin `((set! ,u ,lhs) ,effect))))]
				[(mset! ,base ,offset ,value)
					(cond
						[(label? value) (let* ((new-value (new-u)) (effect (Effect `(mset! ,base ,offset ,new-value))))
							(make-begin `((set! ,new-value ,value) ,effect)))]
						[(or (and (ur? base) (ur? offset) (ur? value))
									(and (ur? base) (ur? offset) (integer? value))
								 (and (ur? base) (int32? offset) (int32? value))
								 (and (ur? base) (int32? offset) (ur? value))
								 (and (int32? base) (ur? offset) (ur? value))
								 (and (ur? base) (int32? offset) (label? value))) ef]
						[(and (ur? base) (int32? offset) (frame-var? value))
							(let ((u (new-u)))
								`(begin (set! ,u ,value) (mset! ,base ,offset ,u)))]
						[(and (frame-var? base) (ur? offset) (frame-var? value))
								(let ((new-base (new-u)) (new-value (new-u)))
									`(begin (set! ,new-base ,base) (set! ,new-value ,value) (mset! ,new-base ,offset ,new-value)))]
						[(or
							(and (frame-var? base) (int32? offset) (int32? value))
							(and (frame-var? base) (int32? offset) (ur? value)))
								(let ((u (new-u)))
									`(begin (set! ,u ,base) (mset! ,u ,offset ,value)))]
						[else (error who "invalid Mref: ~s" ef)])]
			