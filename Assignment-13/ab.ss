;;; Shiv Indap
;;; P523
;;; sindap
;;; Assignment-B

;;; Optimize self-reference is useful when aa function is being passed itself as a parameter, optimize-self reference will look for such functions
;;; and replace the paramter directly with a function pointer this will also save some additional pointers from being stored

(define-who optimize-self-reference
	;;; builds an initial list of functions which are self-referenced
	(define get-self-reference
		(lambda (element)
			(let ([func-name (car element)] [func-params (cddr element)])
				(if (memq func-name func-params)
						`(,(unique-label func-name) . ,func-name) '()))))
	(define primitives
  	'(+ - * <= < = >= > procedure? boolean? car cdr cons eq? fixnum? make-vector null? pair? set-car! set-cdr! vector? vector-length vector-ref vector-set! void ))
 ;;; Called when (,expr ,expr* ...) is found, we need to evaluate each param, if it is a uvar we check if it is a function being referenced and replace
 ;;; that uvar with its corresponding function-pointer, I couldnot use map or union or intersection because the order in which the parameters are inserted
 ;;; matters
	(define Process-params
		(lambda (param* uvar env)
			(cond
				[(null? param*) '()]
				[else (let* ([first (car param*)]
										 [present (assq first env)])
										 (if present
												 (cons (caddr present) (Process-params (cdr param*) uvar env))
												 (if (eq? uvar first) 
													(Process-params (cdr param*) uvar env)
													(cons first (Process-params (cdr param*) uvar env)))))])))
  ;;; In case a uvar that is being self-refernced in the closures () form I replace that uvar with its corresponding closurepointer
  (define sanitize-closure
		(lambda (closure* env)
			(lambda (closure)
				(match closure
					[(,uvar ,label ,param* ...)
							(let ([intersect (intersection param* (map cdr closure*))])
								(if (null? intersect)
									`(,uvar ,label ,param* ...)
									 (let ([params (Process-params param* uvar env)])
											`(,uvar ,label ,params ...))))]))))
	;;; Expr helper takes 2 parameters, the first being a list of all functions self-referenced, and the next is a list of function-uvars and their 
	;;; corresponding functionpointers
	(define Expr
		(lambda (closure* env)
			(lambda (expr)
				(match expr
					[(if ,[(Expr closure* env) -> test] ,[(Expr closure* env) -> conseq] ,[(Expr closure* env) -> alt]) `(if ,test ,conseq ,alt)]
					[(quote ,im) `(quote ,im)]
					[(begin ,[(Expr closure* env) -> expr*] ... ,[(Expr closure* env) -> tail]) `(begin ,expr* ... ,tail)]
					[(letrec (,[(Body closure* '())-> body*] ...)
											(closures (,[(sanitize-closure closure* env) -> clos*] ...) 
												,[(Expr closure* env) -> tail]))
										`(letrec (,body* ...)
												(closures (,clos* ...)
													,tail))]
					[(let ([,uvar* ,[(Expr closure* env) -> exp*]] ...) ,[(Expr closure* env) -> tail]) `(let ([,uvar* ,exp*] ...) ,tail)]
					[(,prim ,[(Expr closure* env) -> expr*] ...) (guard (memq prim primitives))  `(,prim ,expr* ...)]
					[(,[(Expr closure* env) -> expr-a] ,[(Expr closure* env) -> expr] ,[(Expr closure* env) -> rem*] ...)
							`(,expr-a ,(Process `(,expr ,rem* ...) env) ...)]
					[,x (guard (or (uvar? x) (label? x))) x]))))
	(define Process
		(lambda (expr env)
			(cond 
				[(null? expr) '()]
				[(uvar? (car expr)) (let ([first (car expr)])
															(if (assq first env)
																	(cons (caddr (assq first env)) (Process (cdr expr) env))
																	(cons first (Process (cdr expr) env))))]
				[else (cons (car expr) (Process (cdr expr) env))])))
	;;; Body form handles expressions of the form (,label (lambda() ....)), I needed to get each label and corresponding lambda expressions
	;;; thats why I created this Helper
	(define Body
		(lambda (closure* env)
			(lambda (body)
				(match body
					[(,label (lambda (,fptr ,param* ...)
											(bind-free (,free* ...)
												,tail)))
							(let ([ispresent? (assq label closure*)]) ;;;check if this function has a self reference
								;;;if present change the env and record its closurepointer 
								(if ispresent?
									(let* ([uvar (cdr ispresent?)]
												 [new-env (cons `(,uvar . (,label ,fptr)) env)])
										`(,label (lambda (,fptr ,param* ...)
																(bind-free (,(remq uvar free*) ...)
																	,((Expr closure* new-env) tail)))))
										;;; if not dont modify the enviornment pass it on as is
										`(,label (lambda (,fptr ,param* ...)
																(bind-free (,free* ...)
																	,((Expr closure* env) tail))))))]))))
	;;; traverses over the entire program and gets a list of functions that are self referenced
	(define collect-closures
		(lambda (expr)
			(match expr
				[(if ,[collect-closures -> test] ,[collect-closures -> conseq] ,[collect-closures -> alt]) `(,test ... ,conseq ... ,alt ...)]
				[(quote ,x) '()]
				[(begin ,[collect-closures -> expr*] ... ,[collect-closures -> tail]) `(,expr* ... ...,tail ...)]
				[(letrec ([,label* (lambda (,param* ...) (bind-free (,fptr ,free* ...) ,[collect-closures -> body*]))] ...) 
								(closures (,func* ...) ,[collect-closures -> tail])) 
								`(,body* ... ...,func* ... ,tail ...)]
				[(let ([,uvar* ,[collect-closures -> exp*]] ...) ,[collect-closures -> tail]) `(,exp* ... ...,tail ...)]
				[(,prim ,[collect-closures -> expr*] ...) (guard (memq prim primitives)) `(,expr* ... ...)]
				[(,[collect-closures -> expr-a] ,[collect-closures -> expr] ,[collect-closures -> rem*] ...) `(,expr-a ...,expr ...,rem* ... ...)]
				[,x (guard (or (uvar? x) (label? x))) '()])))
	(lambda (x)
		(let* ([closure-list (collect-closures x)]
			 		 [self-refer-closures (remq '() (map get-self-reference closure-list))])
					 ((Expr self-refer-closures '()) x))))