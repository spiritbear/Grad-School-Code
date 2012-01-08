; Finalize locations Pass
;------------------------------------------------------------------------------------------------------------------------------------------------------
(define finalize-locations
	(lambda (prog)
		(define build-funcs
			(lambda (label* body*)
				(map build-function label* body*)))
		(define build-function
			(lambda (label body)
				`(,label (lambda () ,(Body body)))))
		(define build-env
			(lambda (uvar* loc*)
				(map (lambda (x y) (cons x y)) uvar* loc*)))
		(define replace-uvar
			(lambda (effect* env)
				(map (lambda (proc) (proc env)) (map Effect effect*))))
		(define Program
			(lambda (x)
				(match x
					[(letrec ([,label* (lambda () ,body*)] ...) ,body) 
						(let ((letrec-body (Body body)) (funcs (build-funcs label* body*)))
							`(letrec ,funcs ,letrec-body))])))
		(define Body
			(lambda (x)
				(match x
					[(locate ([,uvar* ,loc*] ...) ,tail) 
						(let ((env (build-env uvar* loc*)))
							(Tail tail env))])))
		(define Effect
			(lambda (effect)
				(lambda (env)
					(match effect
						[(begin ,effect* ... ,effect)
						 	(let ((effects (replace-uvar effect* env)) (parsed-tail ((Effect effect) env)))
								`(begin ,@effects ,parsed-tail))]
						[(set! ,var (,binop ,triv1 ,triv2)) `(set! ,(Var var env) (,binop ,(Triv triv1 env) ,(Triv triv2 env)))]
						[(set! ,var ,triv) `(set! ,(Var var env) ,(Triv triv env))]
						[(nop) '(nop)]
						
						[(if ,pred ,conseq ,alt) `(if ,(Pred pred env) ,((Effect conseq) env) ,((Effect alt) env))]))))
		(define Pred
			(lambda (x env)
				(match x
					[(true) (list 'true)]
					[(false) (list 'false)]
					[(,relop ,triv1 ,triv2) `(,relop ,(Triv triv1 env) ,(Triv triv2 env))]
					[(if ,conde ,conseq ,alt) `(if ,(Pred conde env) ,(Pred conseq env) ,(Pred alt env))])))
		(define Triv
			(lambda (x env)
				(if (or (integer? x) (label? x)) x (Var x env))))
		(define Var
			(lambda (x env)
				(if (uvar? x) (cdr (assq x env)) (Loc x))))
		(define Loc
			(lambda (x)
				x))
		(define Tail
			(lambda (x env)
				(match x
					[(begin ,effect* ... ,tail)
						 (let ((effects (replace-uvar effect* env)) (parsed-tail (Tail tail env)))
							`(begin ,@effects ,parsed-tail))]
					[(if ,pred ,tail1 ,tail2) `(if ,(Pred pred env) ,(Tail tail1 env) ,(Tail tail2 env))]
					[(,triv) (list (Triv triv env))])))
	(Program prog)))

;expose-frame-var pass
;------------------------------------------------------------------------------------------------------------------------------------------------------
	(define expose-frame-var
		(lambda (prog)
			(define build-funcs
				(lambda (label* body*)
					(map build-function label* body*)))
			(define build-function
				(lambda (label body)
					`(,label (lambda () ,(Tail body)))))
		(define Program
			(lambda (x)
				(match x
					[(letrec ([,label* (lambda () ,tail*)] ...) ,tail) 
						(let ((letrec-tail (Tail tail)) (funcs (build-funcs label* tail*)))
							`(letrec ,funcs ,letrec-tail))])))
		(define Effect
			(lambda (effect)
				(match effect
					[(begin ,effect* ... ,effect)
						(let ((effect*-parsed (map Effect effect*)) (effect-parsed (Effect effect)))
							`(begin ,@effect*-parsed ,effect-parsed))]
					[(set! ,loc (,binop ,triv1 ,triv2)) `(set! ,(Loc loc) (,binop ,(Triv triv1) ,(Triv triv2)))]
					[(set! ,loc ,triv) `(set! ,(Loc loc) ,(Triv triv))]
				  [(nop) '(nop)]
					[(if ,pred ,conseq ,alt) `(if ,(Pred pred) ,(Effect conseq) ,(Effect alt))])))
		(define Pred
			(lambda (x)
				(match x
					[(true) (list 'true)]
					[(false) (list 'false)]
					[(,relop ,triv1 ,triv2) `(,relop ,(Triv triv1) ,(Triv triv2))]
					[(if ,conde ,conseq ,alt) `(if ,(Pred conde) ,(Pred conseq) ,(Pred alt))]
					[(begin ,effect* ..., body)
						(let ((effect*-parsed (map Effect effect*)) (pred-parsed (Pred body)))
							`(begin ,@effect*-parsed ,pred-parsed))])))
		(define Triv
			(lambda (x)
				(if (or (integer? x) (label? x)) x (Loc x))))
		(define Loc
			(lambda (x)
				(if (frame-var? x) (make-disp-opnd 'rbp (ash (frame-var->index x) 3))
				x)))
		(define Tail
			(lambda (x)
				(match x
					[(begin ,effect* ... ,tail) 
						(let ((effect*-parsed (map Effect effect*)) (tail-parsed (Tail tail)))
							`(begin ,@effect*-parsed ,tail-parsed))]
					[(if ,pred ,tail1 ,tail2) `(if ,(Pred pred) ,(Tail tail1) ,(Tail tail2))]
					[(,triv) (list (Triv triv))])))
		(Program prog)))
