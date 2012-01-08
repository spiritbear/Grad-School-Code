(eval-when (compile load eval)
   (optimize-level 2)
   (case-sensitive #t)
 )

 (load "match.ss")
 (load "helpers.ss")
 (load "fmts.pretty")     ; inform pretty-print about new forms
 (load "driver.ss")

 (load "verify-scheme.ss")
 ;(load "finalize-locations.ss")
 ;(load "a3-wrapper.ss")   ; defines syntactic forms and procedures
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
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------



(define expose-basic-blocks
	(lambda (prog)	
		(define function-list '())
		(define build-funcs
			(lambda (label* body*)
				(map build-function label* body*)))
		(define build-function
			(lambda (label body)
				`(,label (lambda () ,(Tail body)))))
		(define build-function*
			(lambda (label body)
				`(,label (lambda () ,body))))
		(define generate-function
			(lambda (list-of-funcs)
				(map (lambda (x) (build-function* (car x) (cdr x))) list-of-funcs)))
		(define Program
			(lambda (x)
				(match x
					[(letrec ([,label* (lambda () ,[Tail -> bindings* tail*])] ...) ,[Tail -> binding tail])
							`(letrec ,(fold-right (lambda(x y z ls) (append (append (bind-function x y) z) ls)) binding label* tail* bindings*) ,tail)
							])))
		(define Tail
			(lambda (x)
				(match x
					[(if ,condition ,[Tail -> cbinding conseq] ,[Tail -> abinding alt]) 
						(let ((conseq-label (unique-label 'c))
									(alt-label (unique-label 'a)))
									(let-values ([(bindings statements) (Pred condition conseq-label alt-label)]) 
										(values (append cbinding abinding bindings (bind-function conseq-label conseq) (bind-function alt-label alt)) statements)))]
					[(begin ,effect* ...,[Tail -> binding tail])
					 		(let-values ([(bindings statements) (Effect* effect* (list tail))])
								(values (append binding bindings) statements))]		
					[(,triv) (values '() `(,triv))])))
		(define Effect*
			(lambda (ef* code)
				(match ef*
					[() (values '() (if (null? code) '() (make-begin (if (list? (car code)) code `(,code)))))]
					[(,effect* ...,effect) (Effect effect* effect code)]))) 
		(define Effect
			(lambda (before* expr after*)
				(match expr
					[(if ,condition ,conseq ,alt)
						(let ((conseq-label (unique-label 'c))
									(alt-label (unique-label 'a))
									(jmp-label (unique-label 'j)))
							(let*-values
								([(p-bindings p-statements) (Pred condition conseq-label alt-label)]
								 [(c-bindings c-statements) (Effect '() conseq (list (list jmp-label)))]
								 [(a-bindings a-statements) (Effect '() alt (list (list jmp-label)))]
								 [(e-bindings e-statements) (Effect* before* (list p-statements))])
								 (values (append (bind-function conseq-label c-statements) 
												 (bind-function alt-label a-statements)
												 (bind-function jmp-label (if (null? after*) '() (make-begin after*))) p-bindings c-bindings a-bindings) e-statements)))]
					
					[(set! ,loc ,triv) (Effect* before* (append (list expr) (if (list? (car after*)) after* `(,after*))))]
					[(begin ,effect* ...) 
						(let*-values 
							([(e-bindings e-statements) (Effect* effect* after*)]
							 [(b-bindings statements) (Effect* before* e-statements)])
							(values (append e-bindings b-bindings) statements))]
				  [(nop) (Effect* before* after*)])))
		(define Pred
				(lambda (exp true-label false-label)
					(match exp
						[(true) (values '() `(,true-label))]
						[(false) (values '() `(,false-label))]
						[(begin ,effect* ..., body) 
							(let*-values
								([(p-bindings p-statements) (Pred body true-label false-label)]
								 [(e-bindings e-statements) (Effect* effect* p-statements)])
								(values (append e-bindings p-bindings) e-statements))]
						[(,relop ,triv1 ,triv2) (values '() `(if ,exp (,true-label) (,false-label)))]
						[(if ,conde ,conseq ,alt) 
							(let ((conseq-label (unique-label 'c))
										(alt-label (unique-label 'a)))
								(let*-values
									([(p-bindings p-statements) (Pred conde conseq-label alt-label)]
									 [(c-bindings c-statements) (Pred conseq true-label false-label)]
									 [(a-bindings a-statements) (Pred alt true-label false-label)])
									 (values (append (bind-function conseq-label c-statements) 
													 (bind-function alt-label a-statements) p-bindings c-bindings a-bindings) p-statements)))])))
		(define bind-function
			(lambda (label statement)
				`((,label (lambda () ,statement)))))						
		(Program prog)))

;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------

(define-who flatten-program
;;; This helper processes the Tail part of above grammar. It takes a list of labels as an argument.
;;; The car of the labels list tells the next letrec binding's label.
  (define Tail
    (lambda (label*)
      (lambda (tail)
        (match tail
          [(,t) (if (or (null? label*) (not (eq? t (car label*)))) `((jump ,t)) '())]
          [(if(,relop ,x ,y) (,lblc) (,lbla))
           (if (null? label*)
               `((if(,relop ,x ,y) (jump ,lblc)) (jump ,lbla))
               (let ([nlbl (car label*)])
                 (if (eq? nlbl lblc)
                     `((if(not (,relop ,x ,y)) (jump ,lbla)))
                     (if (eq? nlbl lbla)
                         `((if(,relop ,x ,y) (jump ,lblc)))
                         `((if(,relop ,x ,y) (jump ,lblc)) (jump ,lbla))))))]
          [(begin ,ef* ... ,[(Tail label*) -> tail])
           (append ef* tail)]))))
;;; This helper procedure takes all labels and the corresponding tails to generate final bindings.
  (define tailcaller
    (lambda (label* tail*)
      (if (null? label*)
          '()
          (append (list (car label*)) ((Tail (cdr label*)) (car tail*)) (tailcaller (cdr label*) (cdr tail*))))))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,tail*)] ...) ,tail)
       (append '(code) ((Tail label*) tail) (tailcaller label* tail*))])))


(define test
	(lambda ()
		(flatten-program
			(expose-basic-blocks
				(expose-frame-var
					(finalize-locations
						'(letrec ([f$1 (lambda ()
				                    (locate ([x.1 r8] [y.2 r9])
				                      (if (if (= x.1 1) (true) (> y.2 1000))
				                          (begin (set! rax y.2) (r15))
				                          (begin
				                            (set! y.2 (* y.2 2))
				                            (set! rax x.1)
				                            (set! rax (logand rax 1))
				                            (if (= rax 0) (set! y.2 (+ y.2 1)) (nop))
				                            (set! x.1 (sra x.1 1))
				                            (f$1)))))])
				      (locate ()
				        (begin
				          (set! r8 3)
				          (set! r9 10)
				          (f$1))))))))))


(define test-1
	(lambda ()
		;(flatten-program
			(expose-basic-blocks
				(expose-frame-var
					(finalize-locations
					'	(letrec ([if-test$5 (lambda ()
				                          (locate ([n.1 rdi] [x.2 rax] [y.3 rbx])
				                            (begin
				                              (set! x.2 1)
				                              (set! y.3 1)
				                              (if (= n.1 0)
				                                  (set! x.2 (+ x.2 y.3))
				                                  (set! y.3 (+ y.3 x.2)))
				                              (set! x.2 n.1)
				                              (if (if (= n.1 y.3) (false) (true))
				                                  (set! n.1 (+ n.1 x.2))
				                                  (set! n.1 (+ n.1 y.3)))
				                              (set! x.2 n.1)
				                              (r15))))])
				      (locate ([n.1 rdi])
				        (begin
				          (set! n.1 1)
				          (if-test$5))))))))));)
 