(define-who uncover-register-conflict
	(define update-conflict-table
		(lambda (var live* ct)
			(let ((conflict-entry (assq var ct)) (live-vars (remq var live*)))
				(set-cdr! conflict-entry (union live-vars (if (null? (cdr conflict-entry)) '() (cdr conflict-entry))))
				(cons var (map (lambda (x)
								(if (uvar? x)
											(let ((x-conflict-entry (assq x ct)))
																(set-cdr! x-conflict-entry (union (list var) (if (null? (cdr x-conflict-entry)) '() (cdr x-conflict-entry))))))
								x) live-vars)))))
  (define Triv
    (lambda (x)
      (if (or (register? x) (uvar? x)) x '())))
  (define Effect*
    (lambda (x live* ct)
      (match x
        [() live*]
        [(,ef* ... ,ef) (Effect* ef* (Effect ef live* ct) ct)]
        [,x (error who "invalid Effect* list ~s" x)])))
  (define Effect
    (trace-lambda Effect(x live* ct)
      (match x
        [(nop) live*]
        [(if ,test ,[c-live*] ,[a-live*])
         (Pred test c-live* a-live* ct)]
        [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
        [(set! ,lhs (,binop ,[Triv -> x-live*] ,[Triv -> y-live*])) (resolve-lhs-double lhs x-live* y-live* live* ct)]
        [(set! ,lhs ,[Triv -> var]) (resolve-lhs-single lhs var live* ct)]
        [,x (error who "invalid Effect list ~s" x)])))
  (define resolve-lhs-double 
		(lambda (lhs x* y* live* ct)
			(cond
				[(and (register? lhs) (uvar? y*)) (update-conflict-table y* (func-2 lhs x* y* live*) ct)]
				[(register? lhs) (if (member lhs live*) (func-2 lhs x* y* live*) live*)]
				[(uvar? lhs)
						(let ((new-live (func-2 lhs x* y* live*)))
							(update-conflict-table lhs new-live ct))]
				[else live*])))
	(define resolve-lhs-single
		(trace-lambda single(lhs rhs live* ct)
			(cond 
				[(register? lhs) (if (member lhs live*) (func-1 lhs rhs live*) (if (null? rhs) live* (set-cons rhs live*)))]
				[(uvar? lhs)
					(let ((new-live (func-1 lhs rhs live*)))
						(update-conflict-table lhs new-live ct))]
				[else (set-cons rhs live*)])))
	(define func-1
		(lambda (lhs rhs live*)
			(if (null? rhs) (difference live* (list lhs)) (set-cons rhs (difference live* (list lhs))))))
	(define func-2
		(lambda (lhs x y live*)
			(cond
				[(null? x) (set-cons y (difference live* (list lhs)))]
				[(null? y) (set-cons x (difference live* (list lhs)))]
				[else (set-cons x (set-cons y (difference live* (list lhs))))])))					
  (define Pred
    (trace-lambda Pred(x t-live* f-live* ct)
      (match x
        [(true) t-live*]
        [(false) f-live*]
        [(if ,test ,[c-live*] ,[a-live*])
         (union c-live* a-live* t-live* f-live*)]
        [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
        [(,predop ,[Triv -> x-live*] ,[Triv -> y-live*])
         (remove-nulls (union (list x-live*) (list y-live*) t-live* f-live*))]
        [,x (error who "invalid Pred ~s" x)])))
  (define Tail
    (trace-lambda Tail(x ct)
      (match x
        [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
        [(if ,test ,[c-live*] ,[a-live*])
         (union c-live* a-live*)]
        [(,target ,live* ...)
         (remove-nulls (cons (Triv target) (map Triv live*)))]
        [,x (error who "invalid Tail ~s" x)])))
  (define remove-nulls
		(lambda (ls)
			(cond
				[(null? ls) '()]
				[(null? (car ls)) (remove-nulls (cdr ls))]
				[else (cons (car ls) (remove-nulls (cdr ls)))])))
  (define Body
    (trace-lambda Body(x)
      (match x
        [(locals (,uvar* ...) ,tail)
         ;; set up the conflict table ct for storing conflicts
         ;; with initial empty list of conflicts for each uvar
         (let ([ct (map (lambda (x) (cons x '())) uvar*)])
           (let ([live* (Tail tail ct)])
             `(locals (,uvar* ...)
                (register-conflict ,ct ,tail))))]
        [,x (error who "invalid Body ~s" x)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,x (error who "invalid Program ~s" x)])))


