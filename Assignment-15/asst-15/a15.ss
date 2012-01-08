;;; Shiv Indap
;;; sindap
;;; Assignment 15


;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------

;;; Parse-Scheme basically checks if the program is compatible with the subset of scheme the compiler is supposed to work on
;;; The Subset of Scheme for The Final Compiler
;;; Program ---->		Expr 
;;;	Expr		---->   constant
;;;							|  	var
;;;							|		(quote datum)
;;;							|		(if Expr Expr)
;;;							|		(if Expr Expr Expr)
;;;							|		(and Expr*)
;;;							|		(or Expr*)
;;;							|		(begin Expr* Expr)
;;;							|		(lambda (uvar*) Expr+)
;;;							|		(let ([var Expr]*) Expr+)
;;;							|		(letrec ([var Expr]*) Expr+)
;;;							|		(set! uvar Expr)
;;;							|		(prim Expr*)
;;;							|		(Expr Expr*)
;;;	where:
;;;
;;;constant is #t, #f, (), or a fixnum;
;;;fixnum is an exact integer;
;;;datum is a constant, pair of datums, or vector of datums; and
;;;var is an arbitrary symbol.

(define-who parse-scheme
	;;;checks if x is #t #f () or fixnum
	(define (datum? x)
 		(or (constant? x)
       					(if (pair? x)
           					(and (datum? (car x)) (datum? (cdr x)))
           					(and (vector? x) (andmap datum? (vector->list x))))))
   ;;; checks if x is a constant
	(define (constant? x)
			(or (memq x '(() #t #f))
				(and (integer? x) (exact? x) (fixnum-range? x))))
	;;; primitives and te number of arguments they are supposed to take
  (define primitives
    '((+ . 2) (- . 2) (* . 2) (<= . 2) (< . 2) (= . 2)
      (>= . 2) (> . 2) (boolean? . 1) (car . 1) (cdr . 1)
      (cons . 2) (eq? . 2) (fixnum? . 1) (make-vector . 1)
      (null? . 1) (pair? . 1) (procedure? . 1) (set-car! . 2)
      (set-cdr! . 2) (vector? . 1) (vector-length . 1)
      (vector-ref . 2) (vector-set! . 3) (void . 0)))
  ;;; since each occurence of a symbol will be replaced by a uvar identified in our language by symbol.suffix eg x -> x.1
	;;; generate returns a tuple of the symbol and the corresponding uvar
  (define generate
		(lambda (uvar)
			`(,uvar . ,(unique-name uvar))))
	;;; if x is a datum process it else return error in Datum expression, will be called only when we encounter an expression of the form
	;;; (quote ,datum)
	(define Datum
		(lambda (x)
			(match x
				[,x (guard (datum? x)) x]
				[,x (error who "error in Datum Expression ~s" x)])))
  ;;; The adjust function implements the shadowing of variables, if we have (letrec x (lambda (x) (+ 5 x)))
  ;;; the x in lambda expression will be shadowed by the parameter in the lambda expression and hence the x in letrec epression will no longer be valid here
	(define adjust
		(lambda (new-list old-list)
			(cond
				[(null? new-list) old-list]
				[else (let* ([x (caar new-list)] [res (assq x old-list)])
										(if res (adjust (cdr new-list) (cons (car new-list) (remq res old-list))) (adjust (cdr new-list) (cons (car new-list) old-list))))])))
	;;; checks if the list of parameters passed to a lambda or the set of bindings for a let and letrec are unique
	(define check-unique
		(lambda (uvar* result* expr)
			(cond
				[(null? uvar*) #t]
				[(memq (car uvar*) result*) (error who "duplicate variables in expression ~s " expr)]
				[else (check-unique (cdr uvar*) (cons (car uvar*) result*) expr)])))
	;;; env is a mapping between symbols and the corresponding uvars applicable to the expression we are now processing
	(define Expr
		(lambda (env)
			(lambda (expr)
				(match expr
					[(begin ,[(Expr env) -> expr*] ... ,[(Expr env) -> expr]) (guard (not (assq 'begin env))) `(begin ,expr* ... ,expr)]
					[(if ,[(Expr env) -> test] ,[(Expr env) -> conseq]) (guard (not (assq 'if env))) `(if ,test ,conseq (void))] ;;; one-armed if's
					[(if ,[(Expr env) -> test] ,[(Expr env) -> conseq] ,[(Expr env) -> alt]) (guard (not (assq 'if env))) `(if ,test ,conseq ,alt)]
					[(letrec ([,uvar* ,expr*] ...) ,tail* ...)
						(guard (not (assq 'letrec env)))
						(if (null? tail*)
								(error who "Tail Expression in ~s must have atleast one value" expr)
								(let* ([unique-name (check-unique uvar* '() expr)] ;;; check if all bindings unique
											 [local-letrec (map generate uvar*)] ;;;create a new-list of symbol to uvar mappings
											 [new-env (adjust local-letrec env)] ;;; add new-list to the existing env implemening shadowing
											 [new-exp* (map (Expr new-env) expr*)] ;;;process the exprs passing the new enviornment
											 [new-tail (map (Expr new-env) tail*)])
											 `(letrec ([,(map cdr local-letrec) ,new-exp*] ...) 
																	,(make-begin new-tail))))]
					[(let ([,uvar* ,expr*] ...) ,tail* ...)
						(guard (not (assq 'let env)))
						(if (null? tail*)
							(error who "Tail Expression in ~s must have atleast one value" expr)
							(let* ([unique-name (check-unique uvar* '() expr)]
									 	 [local-letrec (map generate uvar*)]
										 [new-env (append local-letrec env)]
										 [new-exp* (map (Expr env) expr*)]
										 [new-tail (map (Expr new-env) tail*)])
									 `(let ([,(map cdr local-letrec) ,new-exp*] ...) 
															,(make-begin new-tail))))]
					[(lambda (,uvar* ...) ,tail* ...)
						(guard (not (assq 'lambda env)))
						(if (null? tail*)
							(error who "Tail Expression in ~s must have atleast one value" expr)
							(let* ([unique (check-unique uvar* '() expr)]
										 [local-bindings* (map generate uvar*)]
										 [new-env (adjust local-bindings* env)]
										 [new-tail (map (Expr new-env) tail*)]) ;;; Everytime I encounter a lambda-expression, I create a new enviornment
									   `(lambda (,(map cdr local-bindings*) ...) ,(make-begin new-tail))))]
					[(and ,x* ...)
						(guard (not (assq 'and env)))
						(cond 
							[(null? x*) (quote #t)]
							[(= (length x*) 1) ((Expr env) (car x*))]
							[else `(if ,((Expr env) (car x*)) ,((Expr env) `(and ,(cdr x*) ...)) (quote #f))])]
					[(or ,x* ...)
						(guard (not (assq 'or env)))
						(cond 
							[(null? x*) (quote #f)]
							[(= (length x*) 1) ((Expr env) (car x*))]
							[else 
								(let ([temp (unique-name 't)])
									`(let ([,temp ,((Expr env) (car x*))])
											(if ,temp ,temp ,((Expr env) `(or ,(cdr x*) ...)))))])]						
					[(not ,[(Expr env) -> x]) (guard (not (assq 'not env)))
					 `(if ,x (quote #f) (quote #t))]
					[(set! ,x ,y)
					(guard (not (assq 'set! env)))
						(cond
							[(and (symbol? x) (assq x env)) `(set! ,(cdr (assq x env)) ,((Expr env) y))]
							[else (error who "Either ~s is not a symbol or ~s is not bound" x x)])]
					[(quote ,[Datum -> x]) (guard (not (assq 'quote env))) `(quote ,x)]
					[(,prim ,[(Expr env) -> x*] ...) (guard (and (assq prim primitives) (not (assq prim env))))
						(if (= (length x*) (cdr (assq prim primitives))) 
								`(,prim ,x* ...)
								 (error who "Invalid arguments to primitive ~s " prim))]
					[,x (guard (symbol? x)) 
						(cond
							[(assq x env) (cdr (assq x env))]
							[else (error who "unbound variable ~s " x)])] ;;maybe we will need to replace the uvar by a new expression e.g y will be replaced by y.1
					[,x (guard (constant? x)) `(quote ,x)]
					[(,[(Expr env) -> x] ,[(Expr env) -> x*] ...) `(,x ,x* ...)]
					[,x (error who "Invalid Expression ~s" x)]))))
	(lambda (x)
		((Expr '()) x)))
			
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------


;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------

(define-who convert-complex-datum
	(define primitives
  	'(+ - * <= < = >= > procedure? boolean? car cdr cons eq? fixnum? make-vector null? pair? set-car! set-cdr! vector? vector-length vector-ref vector-set! void))
	(define Expr
		(lambda (expr)
			(match expr
				[,x (guard (uvar? x)) (values x '())]
				[(quote ,[Datum -> datum c-exp]) (values datum c-exp)]
				[(if ,[Expr -> test c-test] ,[Expr -> conseq c-conseq] ,[Expr -> alt c-alt])
							(values `(if ,test ,conseq ,alt) `(,c-test ... ,c-conseq ... ,c-alt ...))]
				[(begin ,[Expr -> exp* c-exp*] ... ,[Expr -> expr c-exp]) (values `(begin ,exp* ... ,expr) `(,c-exp* ... ...,c-exp ...))]
				[(lambda (,uvar* ...) ,[Expr -> body c-body]) (values `(lambda (,uvar* ...) ,body) `(,c-body ...))]
				[(let ([,uvar* ,[Expr -> exp* c-exp*]] ...) ,[Expr -> tail c-tail]) 					
							(values `(let ([,uvar* ,exp*] ...) ,tail) `(,c-exp* ... ... ,c-tail ...))]
				[(letrec ([,uvar* ,[Expr -> exp* c-exp*]] ...) ,[Expr -> tail c-tail])
							(values `(letrec ([,uvar* ,exp*] ...) ,tail) `(,c-exp* ... ... ,c-tail ...))]
				[(set! ,x ,[Expr -> expr c-exp]) (values `(set! ,x ,expr) c-exp)]
				[(,prim ,[Expr -> expr* c-exp*] ...) (guard (memq prim primitives)) (values `(,prim ,expr* ...) `(,c-exp* ... ...))]
				[(,[Expr -> expr c-exp] ,[Expr -> expr* c-exp*] ...) (values `(,expr ,expr* ...) `(,c-exp ...,c-exp* ... ...))])))
	(define Datum
		(lambda (imm)
			(match imm
				 [,x (guard (memq imm '(#t #f ()))) (values `(quote ,imm) '(()))]
	       [,x (guard (and (integer? imm) (exact? imm))
	        (unless (fixnum-range? imm)
	          (error who "integer ~s is out of fixnum range" imm)))
	        (values `(quote ,imm) '(()))]
				 ;; Return a new-uvar and add the expression to the top-level-let
	       [,x (let ([new-var (unique-name 'u)])
								(values new-var `((,new-var (quote ,x)))))])))
	;;; converts a list to its equivalent cons forms
	(define make-list
		(lambda (x)
			(cond
				[(null? x) (quote '())]
				[(and (not (pair? x)) (not (vector? x))) `(quote ,x)]
				[(vector? (car x))
					(let ([vect (handle-vector (car x))])
 						`(cons ,vect ,(make-list (cdr x))))]
				[(pair? (car x)) `(cons ,(make-list (car x)) ,(make-list (cdr x)))]
				[else `(cons (quote ,(car x)) ,(make-list (cdr x)))])))
	;;; driver for make-vector, just sets the initial parameters such as length, new temporary etc
	(define handle-vector
		(lambda (x)
			(let* ([tmp (unique-name 't)] [len (vector-length x)] [ls (vector->list x)] [result (vector-make ls 0 tmp)])
				`(let [(,tmp (make-vector (quote ,len)))]
							,(make-begin `(,@result ,tmp))))))
  ;;; converts the vector form into make-vector, vector-set! etc
	(define vector-make
		(lambda (ls num tmp)
			(cond
				[(null? ls) '()]
				[(pair? (car ls)) 
					(let ([lst (make-list (car ls))])
						(cons `(vector-set! ,tmp (quote ,num) ,lst) (vector-make (cdr ls) (add1 num) tmp)))]
				[(vector? (car ls))
					(let* ([vect (handle-vector (car ls))])
						(cons `(vector-set! ,tmp (quote ,num) ,vect) (vector-make (cdr ls) (add1 num) tmp)))]
				[else (cons `(vector-set! ,tmp (quote ,num) (quote ,(car ls))) (vector-make (cdr ls) (add1 num) tmp))])))
	;;; checks if its a pair or vector and calls the helpers accordingly.
	(define make-expression
		(lambda (binding)
			(match binding
				[(quote ,x) (guard (pair? x)) (make-list x)]
				[(quote ,y) (guard (vector? y)) (handle-vector y)])))
	(lambda (x)
		(match x
			[,x (let-values ([(expr bindings) (Expr x)])
				(if (null? (remq '() bindings)) expr
					(let* ([final-bindings (remq '() bindings)]
								 [uvars* (map car final-bindings)]
								 [exp*   (map make-expression (map cadr final-bindings))])
								`(let ([,uvars* ,exp*] ...) ,expr))))])))

;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------

(define-who uncover-assigned
	(define primitives
  	'(+ - * <= < = >= > procedure? boolean? car cdr cons eq? fixnum? make-vector null? pair? set-car! set-cdr! vector? vector-length vector-ref vector-set! void ))
	(define Expr
			(lambda (expr)
				(match expr
					[,uvar (guard (uvar? uvar)) (values uvar '())]
					[(if ,[Expr -> cond-expr cond-bindings] ,[Expr -> conseq-expr conseq-bindings] ,[Expr -> alt-expr alt-bindings]) 
	 					(values `(if ,cond-expr ,conseq-expr ,alt-expr) (union cond-bindings conseq-bindings alt-bindings))]
					[(quote ,datum) (values `(quote ,datum) '())]
					[(begin ,[Expr -> exp-stmt* exp-binding*] ... ,[Expr -> tail-stmt tail-binding]) 
	 					(values `(begin ,exp-stmt* ... ,tail-stmt) (union (apply append exp-binding*) tail-binding))]
					[(lambda (,uvar* ...) ,[Expr -> tail sets])
						(let ([assigned-uvars (intersection sets `(,uvar* ...))])
							(values `(lambda (,uvar* ...) (assigned (,assigned-uvars ...) ,tail)) sets))]
					[(letrec ([,uvar* ,[Expr -> exp* binding*]] ...) ,[Expr -> tail binding])
					 	(let ([assigned-uvars (intersection uvar* (union binding (apply append binding*)))])
	 						(values `(letrec ([,uvar* ,exp*] ...) 
												(assigned (,assigned-uvars ...) 
													,tail)) 
										  (difference (union binding (apply append binding*)) uvar*)))]
					[(let ([,uvar* ,[Expr -> exp* binding*]] ...) ,[Expr -> tail binding]) 
	 					(let ([assigned-uvars (intersection uvar* (union binding (apply append binding*)))])
	 						(values `(let ([,uvar* ,exp*] ...) 
												(assigned (,assigned-uvars ...) 
													,tail)) 
										  (difference (union binding (apply append binding*)) uvar*)))]
					[(set! ,x ,[Expr -> rhs rhs-assigned]) (values `(set! ,x ,rhs) (union `(,x) rhs-assigned))]
					[(,prim ,[Expr -> expr* bindings*] ...) (guard (memq prim primitives))  
							(values `(,prim ,expr* ...) (apply append bindings* ))]
					[(,[Expr -> expr binding] ,[Expr -> rem* bindings*] ...) 
					 		(values `(,expr ,rem* ...) (union (apply append bindings*) binding))])))
	(lambda (x)
		(let-values ([(final-expr final-bindings) (Expr x)])
				final-expr)))

;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------

(define-who purify-letrec
	(define primitives
  	'(+ - * <= < = >= > procedure? boolean? car cdr cons eq? fixnum? make-vector null? pair? set-car! set-cdr! vector? vector-length vector-ref vector-set! void ))
	(define lambda-expr?
	 (lambda (expr)
		(match expr
		  [(lambda (,uvar* ...) ,x) #t]
		  [,x #f])))
	(define simple-expr?
		(lambda (expr uvar*)
			(match expr
					[,uvar (guard (uvar? uvar)) (if (memq uvar uvar*) #f #t)]
					[(if ,[test] ,[conseq] ,[alt]) (and test conseq alt)]
					[(quote ,datum) #t]
					[(begin ,[expr*] ... ,[tail]) (and expr* tail)]
					[(set! ,x ,[rhs]) (if (memq x uvar*) #f rhs)]
					[(,prim ,[expr*] ...) (guard (memq prim primitives))  (and expr*)]
					[(,[expr] ,[rem*] ...) (and expr rem*)]
					[,x #f])))
	(define Expr
			(lambda (expr)
				(match expr
					[,uvar (guard (uvar? uvar)) uvar]
					[(if ,[Expr -> test] ,[Expr -> conseq] ,[Expr -> alt]) `(if ,test ,conseq ,alt)]
					[(quote ,datum) `(quote ,datum)]
					[(begin ,[Expr -> expr*] ... ,[Expr -> tail]) `(begin ,expr* ... ,tail)]
					[(lambda (,uvar* ...) 
						(assigned (,assign* ...) 
							,[Expr -> tail])) `(lambda (,uvar* ...) 
									(assigned (,assign* ...) ,tail))]
					[(letrec ([,uvar* ,[Expr -> exp*]] ...)
					 		(assigned (,assign* ...)
								,[Expr -> tail])) 
								 (let ([all-pure (seperate-lambdas assign* uvar* exp*)])
										(if (eq? all-pure #t)
											`(letrec ([,uvar* ,exp*] ...)
													,(if (null? assign*) `(,tail ...) `(assigned (,assign* ...) ,tail))) ;;;If the list of assigned is null then we dont add it to the output
													;;; Hence I will be dealing with 2 different letrec forms in the output
											 (let* ([new* (map generate-uvar uvar*)]
														 [new-set! (map generate-set! new* uvar*)])
												`(let ([,uvar* (void)] ...)
														(assigned (,uvar* ...)
															(begin
																(let ([,new* ,exp*] ...)
																	(assigned ()
																		,(make-begin new-set!)))
																			,tail))))))]
					[(let ([,uvar* ,[Expr -> exp*]] ...)
					 		(assigned (,assign* ...)
								,[Expr -> tail])) 
 							`(let ([,uvar* ,exp*] ...) 
										(assigned (,assign* ...) 
											,tail))]
					[(set! ,x ,[Expr -> rhs]) `(set! ,x ,rhs) ]
					[(,prim ,[Expr -> expr*] ...) (guard (memq prim primitives))  `(,prim ,expr* ...)]
					[(,[Expr -> expr] ,[Expr -> rem*] ...) `(,expr ,rem* ...) ])))
	(define seperate-lambdas ;;; traverses over the exps bound by letrec to see if they all are pure, if they are then true else false
		(lambda (assign* uvar* exp*)
			(cond
				[(and (null? assign*) (null? uvar*)) #t]
				;;;Lambda Expression
				[(and (null? assign*) (lambda-expr? (car exp*))) (seperate-lambdas assign* (cdr uvar*) (cdr exp*))]
				[else #f])))
	(define generate-set! 	;;; generates the set! in the inner-let expression
		(lambda (x y)
				`(set! ,y ,x)))
	(define generate-uvar 	;;x.5 will be converted into a new unique also beginning with x
		(lambda (uvar)
			(unique-name (string->symbol (extract-root uvar)))))
	(lambda (x)
		(Expr x)))

;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------

(define-who convert-assignments
	(define primitives
  	'(+ - * <= < = >= > procedure? boolean? car cdr cons eq? fixnum? make-vector null? pair? set-car! set-cdr! vector? vector-length vector-ref vector-set! void ))
	(define Expr
			(lambda (assigned*)
				(lambda (expr)
					(match expr
						;;; check if uvar has been assigned, if so replace it by its car else return as is
						[,uvar (guard (uvar? uvar)) 
							(if (memq uvar assigned*)
								`(car ,uvar)
								 uvar)]
						[(if ,[(Expr assigned*) -> test] ,[(Expr assigned*) -> conseq] ,[(Expr assigned*) -> alt]) `(if ,test ,conseq ,alt)]
						[(quote ,datum) `(quote ,datum)]
						[(begin ,[(Expr assigned*) -> expr*] ... ,[(Expr assigned*) -> tail]) `(begin ,expr* ... ,tail)]
						[(lambda (,uvar* ...) 
							(assigned (,assign* ...) 
								,[(Expr (union assigned* assign*))-> tail]))
								;;Check if any of the parameters has been set!, if so we have to replace it by the new let form else return the lambda expression as is
								 (let ([intersect (intersection uvar* assign*)])
									(if (null? intersect) 
										`(lambda (,uvar* ...) 
												,tail)
												 (let* ([new-bindings (map generate-uvar intersect)]
																[new* (map cdr new-bindings)]
													      [new-lets (make-lets new-bindings intersect)])
												`(lambda (,new* ...)
														(let ,new-lets
																,tail)))))]
					[(letrec ([,uvar* ,[(Expr assigned*)-> exp*]] ...)
								,[(Expr assigned*) -> tail]) 
								`(letrec ([,uvar* ,exp*] ...)
										,tail)]
					;;; Letrec form is returned as is 
					;;; 2 forms were used to account for those expressions that dont have an assigned form encapsulated around the tail							
					[(letrec ([,uvar* ,[(Expr assigned*)-> exp*]] ...)
					 		(assigned (,assign* ...)
								,[(Expr (union assigned* assign*)) -> tail])) 
								`(letrec ([,uvar* ,exp*] ...)
										,tail)]
					[(let ([,uvar* ,[(Expr assigned*) -> exp*]] ...)
					 		(assigned (,assign* ...)
								,[(Expr (union assign* assigned*))-> tail]))
								(if (null? assign*)
									`(let ([,uvar* ,exp*] ...) ,tail)
								 (let* ([new-bindings (map generate-uvar assign*)]
												[new* (map cdr new-bindings)]
												[outer-let* (make-outer-lets uvar* assign* new-bindings `((,uvar* ,exp*) ...))]
									      [new-lets (make-lets new-bindings assign*)])
								`(let ,outer-let*
										(let ,new-lets
												,tail))))]
					[(set! ,x ,[(Expr assigned*) -> rhs]) `(set-car! ,x ,rhs)]
					[(,prim ,[(Expr assigned*) -> expr*] ...) (guard (memq prim primitives))  `(,prim ,expr* ...)]
					[(,[(Expr assigned*) -> expr] ,[(Expr assigned*) -> rem*] ...) `(,expr ,rem* ...) ]))))
	;;; make-outer-let basically assigns the new-uvars to the previous uvars expressions, and in case they have not been set!-ed returns
	;;; the expressions as is 
	(define make-outer-lets
		(lambda (uvar* assign* new* exp*)
			(cond
				[(null? uvar*) '()]
				[else
					(let* ([current-var (car uvar*)])
						(cond
							[(memq current-var assign*) 
								(let ([new-assign (cdr (assq current-var new*))] [old-assign (cadr (assq current-var exp*))])
									(cons `(,new-assign ,old-assign) (make-outer-lets (cdr uvar*) assign* new* exp*)))]
							[else (cons (assq current-var exp*) (make-outer-lets (cdr uvar*) assign* new* exp*))]))])))
	;;; inner-lets where we have (new-uvar (cons old-uvar (void)))
	(define make-lets
		(lambda (new* assign*)
			(cond 
				[(null? assign*) '()]
				[else
					(let* ([current (car assign*)] [value (cdr (assq current new*))])
						(cons `(,current (cons ,value (void))) (make-lets new* (cdr assign*))))])))
	(define generate-uvar
		(lambda (uvar)
			(let ([unique (unique-name (string->symbol (extract-root uvar)))])
				`(,uvar . ,unique))))
	(lambda (x)
		((Expr '()) x)))

;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;;  Program --> <Expr>
;;;  Expr    --> <uvar>
;;;           |  (quote <Immediate>)
;;;           |  (if <Expr> <Expr> <Expr>)
;;;           |  (begin <Expr>* <Expr>)
;;;           |  <Lambda>
;;;           |  (let ([<uvar> <Expr>]*) <Expr>)
;;;           |  (letrec ([<uvar> <Lambda>]*) <Expr>)
;;;           |  (<primitive> <Expr>*)
;;;           |  (<Expr> <Expr>*)
;;;  Lambda  --> (lambda (<uvar>*) <Expr>)
;;;  Immediate -> <fixnum> | () | #t | #f
;;; I am examining the application pattern of the function i.e if rator is a lambda expression and the parameters
;;; passed to it and the parameters it takes are the same, if so we convert the lambda to a let erxpression else
;;; we let it remain a lambda expression


(define-who optimize-direct-call
	(define primitives
  	'(+ - * <= < = >= > procedure? boolean? car cdr cons eq? fixnum? make-vector null? pair? set-car! set-cdr! vector? vector-length vector-ref vector-set! void))
  (define lambda-expr?
	 (lambda (expr)
		(match expr
		  [(lambda (,uvar* ...) ,x) #t]
		  [,x #f])))
  (define (Immediate imm)
     (cond
       [(memq imm '(#t #f ())) imm]
       [(and (integer? imm) (exact? imm))
        (unless (fixnum-range? imm)
          (error who "integer ~s is out of fixnum range" imm))
        imm]
       [else (error who "invalid Immediate ~s" imm)]))
	(define Expr
		(lambda (expr)
			(match expr
				[(if ,[Expr -> test] ,[Expr -> conseq] ,[Expr -> alt]) `(if ,test ,conseq ,alt)]
				[(quote ,[Immediate -> im]) `(quote ,im)]
				[(let ([,uvar* ,[Expr -> exp*]] ...) ,[Expr -> tail]) `(let ([,uvar* ,exp*] ...) ,tail)]
				[(begin ,[Expr -> exp*] ... ,[Expr -> exp]) `(begin ,exp* ... ,exp)]
				[(letrec ([,uvar* (lambda (,param* ...) ,[Expr -> tail*])] ...) ,[Expr -> tail]) `(letrec ([,uvar* (lambda (,param* ...) ,tail*)] ...) ,tail)]
				[(lambda (,uvar* ...) ,x) (Lambda expr)]
				[(,prim ,[Expr -> x*] ...) (guard (memq prim primitives)) `(,prim ,x* ...)]
				[(,x ,[Expr -> y*] ...) (guard (lambda-expr? x)) (convert-lambda x y*)]
				[(,[Expr -> x] ,[Expr -> y*] ...) `(,x ,y* ...)]
				[,x (guard (uvar? x)) x])))
	(define convert-lambda
		(lambda (oper exp*)
			(match oper
				[(lambda (,uvar* ...) ,[Expr -> x])
					(if (= (length uvar*) (length exp*)) `(let ([,uvar* ,exp*] ...) ,x) `(lambda (,uvar* ...) ,x))])))
	(define Lambda
		(lambda (expr)
			(match expr
				[(lambda (,uvar* ...) ,[Expr -> x]) `(lambda (,uvar* ...) ,x)])))
	(lambda (x)
		(Expr x)))


;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------

		
(define-who remove-anonymous-lambda
	(define primitives
  	'(+ - * <= < = >= > procedure? boolean? car cdr cons eq? fixnum? make-vector null? pair? set-car! set-cdr! vector? vector-length vector-ref vector-set! void))
  (define lambda-expr?
	 (lambda (expr)
		(match expr
		  [(lambda (,uvar* ...) ,x) #t]
		  [,x #f])))
  (define (Immediate imm)
     (cond
       [(memq imm '(#t #f ())) imm]
       [(and (integer? imm) (exact? imm))
        (unless (fixnum-range? imm)
          (error who "integer ~s is out of fixnum range" imm))
        imm]
       [else (error who "invalid Immediate ~s" imm)]))
	(define Expr
		(lambda (flag)
			(lambda (expr)
				(match expr
					[(if ,[(Expr 1)-> test] ,[(Expr 1) -> conseq] ,[(Expr 1) -> alt]) `(if ,test ,conseq ,alt)]
					[(quote ,[Immediate -> im]) `(quote ,im)]
					[(let ([,uvar* ,[(Expr 0) -> exp*]] ...) ,[(Expr 1) -> tail]) `(let ([,uvar* ,exp*] ...) ,tail)]
					[(begin ,[(Expr 1) -> exp*] ... ,[(Expr flag) -> exp]) `(begin ,exp* ... ,exp)]
					[(letrec ([,uvar* (lambda (,param* ...) ,[(Expr 1) -> tail*])] ...) ,[(Expr flag) -> tail]) `(letrec ([,uvar* (lambda (,param* ...) ,tail*)] ...) ,tail)]
					[(lambda (,uvar* ...) ,x) (Lambda expr flag)]
					[(,prim ,[(Expr 1) -> x*] ...) (guard (memq prim primitives)) `(,prim ,x* ...)]
					[(,[(Expr 1) -> x] ,[(Expr 1) -> y*] ...) `(,x ,y* ...)]
					[,x (guard (uvar? x)) x]))))				
	(define Lambda
		(lambda (exp flag)
			(match exp
				[(lambda (,uvar* ...) ,[(Expr 1) -> x])
			     (if (eq? flag 0) 
							`(lambda (,uvar* ...) ,x)
							 (let ([anon-var (unique-name 'anon)])
								`(letrec ([,anon-var (lambda (,uvar* ...) ,x)]) ,anon-var)))])))
	(lambda (x)
		((Expr 0) x)))

;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------

(define-who sanitize-binding-forms
	(define primitives
  	'(+ - * <= < = >= > procedure? boolean? car cdr cons eq? fixnum? make-vector null? pair? set-car! set-cdr! vector? vector-length vector-ref vector-set! void))
  ;;; determines if the expresion is a lambda expression
  (define lambda-expr?
	 (lambda (expr)
		(match expr
		  [(lambda (,uvar* ...) ,x) #t]
		  [,x #f])))
  (define (Immediate imm)
     (cond
       [(memq imm '(#t #f ())) imm]
       [(and (integer? imm) (exact? imm))
        (unless (fixnum-range? imm)
          (error who "integer ~s is out of fixnum range" imm))
        imm]
       [else (error who "invalid Immediate ~s" imm)]))
  (define map2
	 (lambda (fn x* y*)
		 (cond
			[(null? x*) '()]
			[else (cons (fn (car x*) (car y*)) (map2 fn (cdr x*) (cdr y*)))])))
	(define Expr
		(lambda (expr)
			(match expr
				[(if ,[Expr -> test] ,[Expr -> conseq] ,[Expr -> alt]) `(if ,test ,conseq ,alt)]
				[(quote ,[Immediate -> im]) `(quote ,im)]
				;;; The function list returns all labels and their corresponding functions in a let expression
				;;; The variables list returns all labels and non-function expressions in a let definition
				[(let ([,uvar* ,[Expr -> exp*]] ...) ,[Expr -> tail])
					(let* ([functions-list (map2 (lambda (uvar exp)
						 											(if (lambda-expr? exp) `(,uvar  ,exp) '())) uvar* exp*)]
						    [variables-list (map2 (lambda (uvar exp)
																	(if (lambda-expr? exp) '() `(,uvar  ,exp))) uvar* exp*)]
								[functions (remq '() functions-list)]
								[variables (remq '() variables-list)])
								(cond
									[(and (null? variables) (null? functions)) tail] ;;;if there are no bindings return tail
									[(null? functions) `(let ,variables ,tail)] ;;;if there are no function-bindings return only let expression
									[(null? variables) `(letrec ,functions ,tail)] ;;;if there are no non-function-bindings return only letrec expression
									[else `(letrec ,functions (let ,variables ,tail))]))] ;;;return a combination of both
				[(begin ,[Expr -> exp*] ... ,[Expr -> exp]) `(begin ,exp* ... ,exp)]
				[(letrec ([,uvar* (lambda (,param* ...) ,[Expr -> tail*])] ...) ,[Expr -> tail]) `(letrec ([,uvar* (lambda (,param* ...) ,tail*)] ...) ,tail)]
				[(lambda (,uvar* ...) ,x) (Lambda expr)]
				[(,prim ,[Expr -> x*] ...) (guard (memq prim primitives)) `(,prim ,x* ...)]
				[(,x ,[Expr -> y*] ...) (guard (lambda-expr? x)) (convert-lambda x y*)]
				[(,[Expr -> x] ,[Expr -> y*] ...) `(,x ,y* ...)]
				[,x (guard (uvar? x)) x])))
	(define convert-lambda
		(lambda (oper exp*)
			(match oper
				[(lambda (,uvar* ...) ,[Expr -> x])
					`(let ([,uvar* ,exp*] ...) ,x)])))
	(define Lambda
		(lambda (expr)
			(match expr
				[(lambda (,uvar* ...) ,[Expr -> x]) `(lambda (,uvar* ...) ,x)])))
	(lambda (x)
		(Expr x)))

;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------

(define-who uncover-free
	(define primitives
  	'(+ - * <= < = >= > procedure? boolean? car cdr cons eq? fixnum? make-vector null? pair? set-car! set-cdr! vector? vector-length vector-ref vector-set! void ))
	(define Expr
			(lambda (expr)
				(match expr
					[,uvar (guard (uvar? uvar)) (values uvar `(,uvar))]
					[(if ,[Expr -> cond-expr cond-bindings] ,[Expr -> conseq-expr conseq-bindings] ,[Expr -> alt-expr alt-bindings]) 
	 					(values `(if ,cond-expr ,conseq-expr ,alt-expr) (union cond-bindings conseq-bindings alt-bindings))]
					[(quote ,[Immediate -> im-expr]) (values `(quote ,im-expr) '())]
					[(begin ,[Expr -> exp-stmt* exp-binding*] ... ,[Expr -> tail-stmt tail-binding]) 
	 					(values `(begin ,exp-stmt* ... ,tail-stmt) (union (apply append exp-binding*) tail-binding))]
					[(letrec ([,uvar* ,[Body -> body* free*]] ...) ,[Expr -> tail-expr tail-binding])
									(values `(letrec ([,uvar* ,body*] ...) ,tail-expr) (difference (union  (apply append free*) tail-binding) uvar*))] 
					[(let ([,uvar* ,[Expr -> exp* binding*]] ...) ,[Expr -> tail binding]) 
	 					(values `(let ([,uvar* ,exp*] ...) ,tail) (difference (union binding (apply append binding*)) uvar*))]
					[(,prim ,[Expr -> expr* bindings*] ...) (guard (memq prim primitives))  
							(values `(,prim ,expr* ...) (apply append bindings* ))]
					[(,[Expr -> expr binding] ,[Expr -> rem* bindings*] ...) 
					 		(values `(,expr ,rem* ...) (union (apply append bindings*) binding))])))
	 (define Body
			(lambda (body)
				(match body
					[(lambda (,param* ...) ,body) 
						(let-values ([(stmt* free*) (Expr body)])
							(let ([free-vars (difference (clean-up free*) param*)])
								(values `(lambda (,param* ...) (free ,free-vars ,stmt*)) free-vars)))])))
	 (define clean-up
		 (lambda (seti)
			 (cond
				[(null? seti) '()]
				[(and (list? (car seti)) (null? (car seti))) (clean-up (cdr seti))]
				[(list? (car seti)) (set-cons (caar seti) (clean-up (cdr seti)))]
				[else (set-cons (car seti) (clean-up (cdr seti)))])))
   (define (Immediate imm)
      (cond
        [(memq imm '(#t #f ())) imm]
        [(and (integer? imm) (exact? imm))
         (unless (fixnum-range? imm)
           (error who "integer ~s is out of fixnum range" imm))
         imm]
        [else (error who "invalid Immediate ~s" imm)]))
	(lambda (x)
		(let-values ([(final-expr final-bindings) (Expr x)])
			(if (not (null? final-bindings))
				(error who "Bindings not null")
				final-expr))))


;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
			
(define-who convert-closures
	(define primitives
  	'(+ - * <= < = >= > procedure? boolean? car cdr cons eq? fixnum? make-vector null? pair? set-car! set-cdr! vector? vector-length vector-ref vector-set! void ))
	(define extract-body
		(lambda (body*)
			(cond
				[(null? body*) '()]
				[else (cons (caar body*) (extract-body (cdr body*)))])))
	(define extract-closures
		(lambda (body*)
			(cond
				[(null? body*) '()]
				[else (cons (cadar body*) (extract-closures (cdr body*)))])))
	(define Expr
			(lambda (expr)
				(match expr
					[,uvar (guard (uvar? uvar)) expr]
					[(if ,[Expr -> test] ,[Expr -> conseq] ,[Expr -> alt]) 
	 						`(if ,test ,conseq ,alt)]
					[(quote ,[Immediate -> im-expr]) `(quote ,im-expr)]
					[(begin ,[Expr -> expr*] ... ,[Expr -> tail]) 
							`(begin ,expr* ... ,tail)]
					[(letrec ([,uvar* ,body*] ...) ,[Expr -> tail])
							(let* ([label* (map Label uvar*)] 
										[process* (map Body uvar* body*)]
										[new-body* (extract-body process*)]
										[local-closures (extract-closures process*)])
								`(letrec ([,label* ,new-body*] ...) (closures (,local-closures ...) ,tail)))] 
					[(let ([,uvar* ,[Expr -> exp*]] ...) ,[Expr -> tail]) 
	 					 `(let ([,uvar* ,exp*] ...) ,tail)]
					[(,prim ,[Expr -> expr*] ...) (guard (memq prim primitives))  
							 	`(,prim ,expr* ...)]
					[(,[Expr -> expr] ,[Expr -> rem*] ...) 
							(if (uvar? expr)
								`(,expr ,expr ,rem* ...)
								 (let ([local (unique-name 't)])
									`(let ([,local ,expr])
											(,local ,local ,rem* ...))))])))
	 (define Body
			(lambda (func-var body)
				(match body
					[(lambda (,param* ...) (free (,free* ...) ,[Expr -> body])) 
						(let ([func-ptr (unique-name 'fp)])
							`((lambda (,func-ptr ,param* ...) (bind-free (,func-ptr ,free* ...) ,body))  (,func-var ,(Label func-var) ,free* ...)))])))
	 (define Label
		(lambda (x)
			(unique-label x)))
   (define (Immediate imm)
      (cond
        [(memq imm '(#t #f ())) imm]
        [(and (integer? imm) (exact? imm))
         (unless (fixnum-range? imm)
           (error who "integer ~s is out of fixnum range" imm))
         imm]
        [else (error who "invalid Immediate ~s" imm)]))
	(lambda (x)
		(Expr x)))


;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------

;;; The strategy is that the function make-closures collects all the definitions enclosed within the closure form,
;;; while traversing over the structure of the program, if we find a uvar which is in rator position
;;; we simply check wether it is a uvar which points to a function, if it is we gebnerate the corresponding label

;;; Grammar for optimize-known-call
;;;
;;;  Program --> <Expr>
;;;  Expr   --> <uvar>
;;;          |  (quote <Immediate>)
;;;          |  (if <Expr> <Expr> <Expr>)
;;;          |  (begin <Expr>* <Expr>)
;;;          |  (let ([<uvar> <Expr>]*) <Expr>)
;;;          |  (letrec ([<uvar> (bind-free (<uvar*) (closures (<closure*>) (lambda (<uvar>*) <Expr>)))]*) <Expr>)
;;;          |  (<primitive> <Expr>*)
;;;          |  (<Expr> <Expr>*)
;;;  Immediate -> <fixnum> | () | #t | #f


(define-who optimize-known-call
	(define primitives
  	'(+ - * <= < = >= > procedure? boolean? car cdr cons eq? fixnum? make-vector null? pair? set-car! set-cdr! vector? vector-length vector-ref vector-set! void ))
	(define Expr
		(lambda (func*)
			(lambda (expr)
				(match expr
					[(if ,[(Expr func*) -> test] ,[(Expr func*) -> conseq] ,[(Expr func*) -> alt]) `(if ,test ,conseq ,alt)]
					[(quote ,[Immediate -> im-expr]) `(quote ,im-expr)]
					[(begin ,[(Expr func*) -> expr*] ... ,[(Expr func*) -> tail]) `(begin ,expr* ... ,tail)]
					[(letrec ([,label* ,[(Body func*)-> body*]] ...) 
									(closures (,clos* ...) ,[(Expr func*) -> tail])) 
					`(letrec ([,label* ,body*] ...) (closures (,clos* ...) ,tail))]
					[(let ([,uvar* ,[(Expr func*) -> exp*]] ...) ,[(Expr func*) -> tail]) `(let ([,uvar* ,exp*] ...) ,tail)]
					[(,prim ,[(Expr func*) -> expr*] ...) (guard (memq prim primitives))  `(,prim ,expr* ...)]
					[(,[(Expr func*) -> expr-a] ,[(Expr func*) -> expr] ,[(Expr func*) -> rem*] ...) 
							 (if (memq expr-a func*) `(,(unique-label expr-a) ,expr ,rem* ...) `(,expr-a ,expr ,rem* ...))]
					[,uvar (guard (uvar? uvar)) uvar]))))
	 (define Body
		 (lambda (func*)
			(lambda (body)
				(match body
					[(lambda (,param* ...) (bind-free (,fptr ,free* ...) ,[(Expr func*)-> body])) 
							`(lambda (,param* ...) (bind-free (,fptr ,free* ...) ,body))]))))
	 (define collect-closures
		(lambda (expr)
			(match expr
				[(if ,[collect-closures -> test] ,[collect-closures -> conseq] ,[collect-closures -> alt]) `(,test ... ,conseq ... ,alt ...)]
				[(quote ,x) '()]
				[(begin ,[collect-closures -> expr*] ... ,[collect-closures -> tail]) `(,expr* ... ...,tail ...)]
				[(letrec ([,label* (lambda (,param* ...) (bind-free (,fptr ,free* ...) ,[collect-closures -> body*]))] ...) 
								(closures (,func* ...) ,[collect-closures -> tail])) 
								`(,body* ... ...,(map car func*) ,tail ...)]
				[(let ([,uvar* ,[collect-closures -> exp*]] ...) ,[collect-closures -> tail]) `(,exp* ... ...,tail ...)]
				[(,prim ,[collect-closures -> expr*] ...) (guard (memq prim primitives)) `(,expr* ... ...)]
				[(,[collect-closures -> expr-a] ,[collect-closures -> expr] ,[collect-closures -> rem*] ...) `(,expr-a ...,expr ...,rem* ... ...)]
				[,uvar (guard (uvar? uvar)) '()])))
   (define (Immediate imm)
      (cond
        [(memq imm '(#t #f ())) imm]
        [(and (integer? imm) (exact? imm))
         (unless (fixnum-range? imm)
           (error who "integer ~s is out of fixnum range" imm))
         imm]
        [else (error who "invalid Immediate ~s" imm)]))
	(lambda (x)
		(let ([closures (apply append (remq '() (collect-closures x)))])
			((Expr closures) x))))

;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
			
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
							(guard enable-optimize-self-reference)
							`(,(Process `(,expr-a ,expr ,rem* ...) env) ... )]
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

;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------

(define-who introduce-procedure-primitives
	(define primitives
  	'(+ - * <= < = >= > procedure? boolean? car cdr cons eq? fixnum? make-vector null? pair? set-car! set-cdr! vector? vector-length vector-ref vector-set! void ))
	(define Expr
		(lambda (fptr free-list)
			(lambda (expr)
				(match expr
					[(if ,[(Expr fptr free-list)-> test] ,[(Expr fptr free-list) -> conseq] ,[(Expr fptr free-list) -> alt]) 
	 						`(if ,test ,conseq ,alt)]
					[(quote ,[Immediate -> im-expr]) `(quote ,im-expr)]
					[(begin ,[(Expr fptr free-list) -> expr*] ... ,[(Expr fptr free-list) -> tail]) 
							`(begin ,expr* ... ,tail)]
					[(letrec ([,label* ,[Body -> body*]] ...) ,[(Tail fptr free-list) -> tail])
						`(letrec ([,label* ,body*] ...) ,tail)]
					[(let ([,uvar* ,[(Expr fptr free-list) -> exp*]] ...) ,[(Expr fptr free-list) -> tail]) 
	 					 `(let ([,uvar* ,exp*] ...) ,tail)]
					[(,prim ,[(Expr fptr free-list) -> expr*] ...) (guard (memq prim primitives))  
							 	`(,prim ,expr* ...)]
					[(,expr-a ,[(Expr fptr free-list) -> expr] ,[(Expr fptr free-list) -> rem*] ...) (guard (label? expr-a))
					 		`(,expr-a ,expr ,rem* ...)]		
					[(,[(Expr fptr free-list) -> expr-a] ,[(Expr fptr free-list) -> expr] ,[(Expr fptr free-list) -> rem*] ...)
								(if (null? free-list)
										  `((procedure-code ,expr-a) ,expr ,rem* ...)
										  (let ([pos (find-pos 0 expr free-list)])
															(if (eq? pos -1) `((procedure-code ,expr-a) ,expr ,rem* ...)
															`((procedure-code (procedure-ref ,fptr (quote ,pos))) 
															  (procedure-ref ,fptr (quote ,pos))
															  ,rem* ...))))]
					[,uvar (guard (uvar? uvar)) 
						(if (null? free-list) uvar
							(let ([pos (find-pos 0 expr free-list)])
								(if (eq? pos -1) uvar
									`(procedure-ref ,fptr (quote ,pos)))))]))))
	 (define make-funcs
		 (lambda (bindfree)
			 (let ([func-uvar (list-ref bindfree 0)]
						 [func-label (list-ref bindfree 1)]
						 [arg-length (length (cddr bindfree))])
						 `(,func-uvar (make-procedure ,func-label (quote ,arg-length))))))
	 (define make-list
			(lambda (start val)
				(cond
					[(> start val) '()]
					[else (cons start (make-list (add1 start) val))])))
	 ;;; finds if a variable exists in the list of free variables passed to that function
	 (define find-pos
		(lambda (pos needle haystack)
			(cond
				[(null? haystack) '-1 ]
				[(eq? needle (car haystack)) pos]
				[else (find-pos (add1 pos) needle (cdr haystack))])))
	 (define make-procs-set!
		 (lambda (closure)
			(lambda (fptr free-vars)
			 (let* ([func-uvar (list-ref closure 0)] ;;;first position is uvar
						 [arg* (cddr closure)] ;;; free vars are everything after the 1st 2 places
						 [arg-length (length arg*)] ;;legth of the list
						 [index-list (make-list '0 (sub1 arg-length))]) ;;generates a list from 0 to length-1
						 ;;; it is necessary to map each and every free variable to an index so that procedure-set!
						;;; can set the correct arguments
						 (map (lambda (x y)
							      (let ([pos (find-pos '0 x free-vars)])
											(if (eq? pos '-1) 
												`(procedure-set! ,func-uvar (quote ,y) ,x)
												`(procedure-set! ,func-uvar (quote ,y) (procedure-ref ,fptr (quote ,pos)))))) arg* index-list)))))
	 (define Tail
		(lambda (fptr free-list)
		 (lambda (tail)
			 (match tail
				 [(closures (,funcs* ...) ,[(Expr fptr free-list) -> new-tail])
						(let ([functions* (map make-funcs funcs*)]
								  ;;; necessary because make-proc-set! returns a list of functions
									;;; so I define another map which applies these functions taking the appropriate 
									;;; parameters
									[proc-set (map (lambda (fn)
																		(fn fptr free-list))(map make-procs-set! funcs*))])
							`(let ,functions* ,(make-begin `(,proc-set ... ...,new-tail))))]))))
	 (define Body
			(lambda (body)
				(match body
					[(lambda (,param* ...) (bind-free (,fptr ,free* ...) ,[(Expr fptr free*) -> body])) 
							`(lambda (,param* ...) ,body)])))
	 (define Label
		(lambda (x)
			(unique-label x)))
   (define (Immediate imm)
      (cond
        [(memq imm '(#t #f ())) imm]
        [(and (integer? imm) (exact? imm))
         (unless (fixnum-range? imm)
           (error who "integer ~s is out of fixnum range" imm))
         imm]
        [else (error who "invalid Immediate ~s" imm)]))
	(lambda (x)
		(set! closure-list '())
		((Expr '() '()) x)))


;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------

(define-who lift-letrec
	;; Defines all the primitives in the Scheme Language
	(define primitives
    '(+ - * <= < = >= > boolean? car cdr cons eq? fixnum? make-vector null? pair? set-car! set-cdr! vector? vector-length vector-ref vector-set! void
		 make-procedure procedure-code procedure-ref procedure-set! procedure?))
	(define definitions '())
	;;; Parses The Expr form of the grammar
	;;; The strategey in the Expr function is to process all the syntactic forms as they are
	;;; letrec is handled differently as it gives us a list of labels and their definitions
	;;; I use side effects to keep a list of definitions and then simply wrap around the tail with a
	;;; letrec form containg all these definitions
	(define Expr
		(lambda (expr)
			(match expr
				[,label (guard (label? label)) label]
				[,uvar (guard (uvar? uvar)) uvar]
				[(if ,[Expr -> cond-expr] ,[Expr -> conseq-expr] ,[Expr -> alt-expr]) 
 					`(if ,cond-expr ,conseq-expr ,alt-expr)]
				[(quote ,[Immediate -> im-expr]) `(quote ,im-expr)]
				[(begin ,[Expr -> exp*] ... ,[Expr -> tail]) 
 					`(begin ,exp* ... ,tail)]
				[(letrec ([,label* (lambda (,param* ...) ,[Expr -> body*])] ...) ,[Expr -> tail])
					(begin
						(set! definitions (append definitions `([,label* (lambda (,param* ...) ,body*)] ...)))
						tail)] 
				[(let ([,uvar* ,[Expr -> exp*]] ...) ,[Expr -> tail]) 
 					`(let ([,uvar* ,exp*] ...) ,tail)]
				[(,prim ,[Expr -> expr*] ...) (guard (memq prim primitives))  
						`(,prim ,expr* ...)]
				[(,[Expr -> exp] ,[Expr -> rem*] ...) 
				 		`(,exp ,rem* ...)])))
		;;;checks if a datatype is of immedeate form and returns it
   (define (Immediate imm)
      (cond
        [(memq imm '(#t #f ())) imm]
        [(and (integer? imm) (exact? imm))
         (unless (fixnum-range? imm)
           (error who "integer ~s is out of fixnum range" imm))
         imm]
        [else (error who "invalid Immediate ~s" imm)]))
	(lambda (x)
		(set! definitions '())
		(let ([tail (Expr x)])
			`(letrec ,definitions ,tail))))


;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------

(define-who normalize-context
	;;; The next 3 functions demarcate the various operators in the context in which they maybe encountered
	(define val-primitive?
		(lambda (x)
			(memq x '(+ * - car cdr cons make-vector vector-length vector-ref make-procedure procedure-ref procedure-code))))
	(define effect-primitive?
		(lambda (x)
			(memq x '(set-car! set-cdr! vector-set! procedure-set!))))
	(define predicate-primitive?
		(lambda (x)
			(memq x '(<= < > = >= boolean? eq? fixnum? null? pair? vector? procedure?))))
	(define Value
		(lambda (val)
			(match val
				[,x (guard (or (label? x) (uvar? x))) x]
				[(quote ,[Immediate -> imm]) `(quote ,imm)]
				[(if ,[Pred -> test] ,[Value -> conseq] ,[Value -> alt]) `(if ,test ,conseq ,alt)]
				[(begin ,[Effect -> ef*] ... ,[Value -> ef]) (make-nopless-begin `(,ef* ... ,ef))]
				[(let ([,uvar* ,[Value -> value*]] ...) ,[Value -> tail]) 
					`(let ([,uvar* ,value*] ...) ,tail)]
				[(void) val] ;;In case void is encountered return its value
				[(,value-prim ,[Value -> val*] ...) (guard (val-primitive? value-prim)) 
					`(,value-prim ,val* ...)]
				[(,pred-prim ,[Value -> val*] ...) (guard (predicate-primitive? pred-prim))
					`(if (,pred-prim ,val* ...) '#t '#f)]
				[(,effect-prim ,[Value -> val*] ...) (guard (effect-primitive? effect-prim)) 
					 	(make-nopless-begin `((,effect-prim ,val* ...) (void)))] ;;Convert to (begin effect* ... (void))
				[(,[Value -> val] ,[Value -> val*] ...) `(,val ,val* ...)]
				)))
	(define Effect
		(lambda (ef)
			(match ef
				[(nop) '(nop)]
				[(void) '(nop)]
				[,x (guard (uvar? x)) '(nop)]
				[(if ,[Pred -> test] ,[Effect -> conseq] ,[Effect -> alt]) `(if ,test ,conseq ,alt)]
				[(begin ,[Effect -> ef*] ... ,[Effect -> ef]) `(begin ,ef* ... ,ef)]
				[(let ([,uvar* ,[Value -> value*]] ...) ,[Effect -> tail]) 
					`(let ([,uvar* ,value*] ...) ,tail)]
				[(,effect-prim ,[Value -> val*] ...) (guard (effect-primitive? effect-prim)) 
				 	`(,effect-prim ,val* ...)]
				[(,value-prim ,val* ...) (guard (or (val-primitive? value-prim) (predicate-primitive? value-prim))) 
				 	'(nop)]
				[(,[Value -> val] ,[Value -> val*] ...) `(,val ,val* ...)])))
	;; Function for Immediate Primitives, we have well-defined values for #t,#f and '()
	;; For integers we shift the value by 3 bits (since we use the lower 3 bits as tag)
	(define Immediate
		(lambda (im)
			(match im
				[#t #t]
				[#f #f]
				[() ()]
				[,t (guard (integer? t)) t])))
	(define Pred
		(lambda (pred)
			(match pred
				[(true) '(true)]
				[(false) '(false)]
				[(quote ,[Immediate -> imm]) `(if (eq? (quote ,imm) '#f) (false) (true))]
				[(if ,[Pred -> test] ,[Pred -> conseq] ,[Pred -> alt]) `(if ,test ,conseq ,alt)]
				[(begin ,[Effect -> ef*] ... ,[Pred -> ef]) `(begin ,ef* ... ,ef) ]
				[(let ([,uvar* ,[Value -> value*]] ...) ,[Pred -> tail]) `(let ([,uvar* ,value*] ...) ,tail)  ]
				[,var (guard (uvar? var)) `(if (eq? ,var '#f) (false) (true))]
				[,label (guard (label? label)) '(true)]
				[(void) `(if (eq? ,pred '#f) (false) (true))]
				[(,pred-prim ,[Value -> val*] ...) (guard (predicate-primitive? pred-prim)) `(,pred-prim ,val* ...)]
				[(,effect-prim ,[Value -> val*] ...) (guard (effect-primitive? effect-prim)) 
				 	(make-nopless-begin `((,effect-prim ,val* ...) (true)))]
				[(,value-prim ,[Value -> val*] ...) (guard (val-primitive? value-prim))
					`(if (eq? (,value-prim ,val* ...) '#f) (false) (true))]
				[(,[Value -> x],[Value -> x*]...) `(if (eq? (,x ,x* ...) '#f) (false) (true))])))
	(lambda (prog)
		(match prog
			[(letrec ([,label* (lambda (,uvar* ...) ,[Value -> val*])] ...) ,[Value -> tail]) 
				`(letrec ([,label* (lambda (,uvar* ...) ,val*)] ...) ,tail)]
			[,x (error who "invalid program ~s" x)])))


;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------

(define (make-nopless-begin x*)
  (let ([x* (remove '(nop) x*)])
    (if (null? x*)
        '(nop)
        (make-begin x*))))

;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------

(define-who specify-representation
	;;; Handles operators that we encounter in <Value> context
	(define handle-operator
		(lambda (rator rand*)
			(cond
				[(eq? rator 'cons) 
					(let ([first (unique-name 't)] [second (unique-name 't)] [third (unique-name 't)])
						`(let ([,first ,(car rand*)] [,second ,(cadr rand*)])
							(let ([,third (+ (alloc ,size-pair) ,tag-pair)])
								(begin
									(mset! ,third ,(- disp-car tag-pair) ,first)
									(mset! ,third ,(- disp-cdr tag-pair) ,second)
									,third))))]
				;;; The - expression is necessary below because when we create a pair we tag it and hence the pointer gets displaced by a value
				;;; given by tag-pair, all we do is subbtract it to get the real value of the pointer
				[(eq? rator 'car)
					`(mref ,(car rand*) ,(- disp-car tag-pair))]
				[(eq? rator 'cdr)
					`(mref ,(car rand*) ,(- disp-cdr tag-pair))]
				;; The arguments to a make-vector can either be a variable or constant and hence the if statemnt is neccessary
				[(eq? rator 'make-vector)
					(let ([size-var (unique-name 't)] [size (car rand*)])
						(if (integer? size)
							`(let ([,size-var (+ (alloc ,(+ disp-vector-data size)) ,tag-vector)])
									(begin
										(mset! ,size-var ,(- disp-vector-length tag-vector) ,size)
										,size-var))
							`(let ([,size-var (+ (alloc (+ ,disp-vector-data ,size)) ,tag-vector)])
									(begin
										(mset! ,size-var ,(- disp-vector-length tag-vector) ,size)
										,size-var))
						))]
				;;; a make-procedure expression looks like (make-procedure label num-args)
				;;; size is specified by the number of args and the func-name is given by func
				;;; I have used the same code used by make-vvector with the few changes I mentioned above
				;;; I should be reporting an error in-case the number-of-args is a variable
				;;; but I am not currently doing so							
				[(eq? rator 'make-procedure)
					(let ([size-var (unique-name 't)] [size (cadr rand*)] [func (car rand*)])
						(if (integer? size)
							`(let ([,size-var (+ (alloc ,(+ disp-procedure-data size)) ,tag-procedure)])
									(begin
										(mset! ,size-var ,(- disp-procedure-code tag-procedure) ,func)
										,size-var))
							`(let ([,size-var (+ (alloc (+ ,disp-procedure-data ,size)) ,tag-procedure)])
									(begin
										(mset! ,size-var ,(- disp-procedure-code tag-procedure) ,func)
										,size-var))
						))]
				[(eq? rator 'vector-length)
					`(mref ,(car rand*) ,(- disp-vector-length tag-vector))]
				;;; similar to vector-length
				[(eq? rator 'procedure-code)
					`(mref ,(car rand*) ,(- disp-procedure-code tag-procedure))]
				[(eq? rator 'vector-ref)
					(let ([value (cadr rand*)])
						(if (integer? value) 
								`(mref ,(car rand*) ,(+ (- disp-vector-data tag-vector) value))
								`(mref ,(car rand*) (+ ,(- disp-vector-data tag-vector) ,value))))]
				;;; similar to vector-ref
				[(eq? rator 'procedure-ref)
					(let ([value (cadr rand*)])
						(if (integer? value) 
								`(mref ,(car rand*) ,(+ (- disp-procedure-data tag-procedure) value))
								`(mref ,(car rand*) (+ ,(- disp-procedure-data tag-procedure) ,value))))]
				[(or (eq? rator '+) (eq? rator '-)) `(,rator ,@rand*)])))
	;;; handles operators that can be encountered in <Effect> context
	;;; since we are using set-car! we need to convert that to mset!
	(define handle-effect-ops
		(lambda (rator rand*)
			(cond
				[(eq? rator 'set-car!) `(mset! ,(car rand*) ,(- disp-car tag-pair) ,(cadr rand*))]
				[(eq? rator 'set-cdr!) `(mset! ,(car rand*) ,(- disp-cdr tag-pair) ,(cadr rand*))]
				[(eq? rator 'vector-set!)
					(let ([value (cadr rand*)])
						(if (integer? value) 
								`(mset! ,(car rand*) ,(+ (- disp-vector-data tag-vector) value) ,(caddr rand*))
				 				`(mset! ,(car rand*) (+ ,(- disp-vector-data tag-vector) ,value) ,(caddr rand*))))]
			 ;;; similar to vector-set!
			  [(eq? rator 'procedure-set!)
					(let ([value (cadr rand*)])
						(if (integer? value) 
								`(mset! ,(car rand*) ,(+ (- disp-procedure-data tag-procedure) value) ,(caddr rand*))
				 				`(mset! ,(car rand*) (+ ,(- disp-procedure-data tag-procedure) ,value) ,(caddr rand*))))])))
	;;; Handles operators that we can encounter in <Effect> context
	(define handle-pred-op
		(lambda (rator rand*)
			(cond 
				[(memq rator '(> < = <= >= )) `(,rator ,@rand*)]
				[(eq? rator 'eq?) `(= ,@rand*)] ;;; eq can be converted to = operator in scheme
				;;; For all other primitives we unmask the values and check if the tag matches the particular data-type
				;;; we are checking for 
				[(eq? rator 'boolean?)
					`(= (logand ,(car rand*) ,mask-boolean) ,tag-boolean)]
				[(eq? rator 'vector?)
					`(= (logand ,(car rand*) ,mask-vector) ,tag-vector)]
				[(eq? rator 'fixnum?)
					`(= (logand ,(car rand*) ,mask-fixnum) ,tag-fixnum)]
				[(eq? rator 'null?)
					(Pred `(eq? ,(car rand*) '()))]
				[(eq? rator 'pair?)
					`(= (logand ,(car rand*) ,mask-pair) ,tag-pair)]
				[(eq? rator 'procedure?)
					`(= (logand ,(car rand*) ,mask-procedure) ,tag-procedure)])))
	(define val-primitive?
		(lambda (x)
			(memq x '(+ - car cdr cons make-vector vector-length vector-ref procedure-ref procedure-code make-procedure))))
	(define effect-primitive?
		(lambda (x)
			(memq x '(set-car! set-cdr! vector-set! procedure-set!))))
	(define predicate-primitive?
		(lambda (x)
			(memq x '(<= < > = >= boolean? eq? fixnum? null? pair? vector? procedure?))))
	(define Value
		(lambda (val)
			(match val
				[,x (guard (or (label? x) (uvar? x) (integer? x))) x]
				[(quote ,[Immediate -> imm]) imm]
				[(mref ,x ,y) val]
				[(if ,[Pred -> test] ,[Value -> conseq] ,[Value -> alt]) `(if ,test ,conseq ,alt)]
				[(begin ,[Effect -> ef*] ... ,[Value -> ef]) `(begin ,ef* ... ,ef)]
				[(let ([,uvar* ,[Value -> value*]] ...) ,[Value -> tail]) 
					`(let ([,uvar* ,value*] ...) ,tail)]
				[(void) $void] ;;In case void is encountered return its value
				;;; Multiplication I have handled using a different case as we dont need to shift both the operands by 8 everytime
				[(* ,[Value -> a] ,[Value -> b])
					(cond
						[(and (integer? a) (integer? b)) `(* ,(sra a shift-fixnum) ,b)]
						[(integer? a) `(* ,b ,(sra a shift-fixnum))]
						[(integer? b) `(* ,a ,(sra b shift-fixnum))]
						[else `(* ,a (sra ,b ,shift-fixnum))])]
				[(,value-prim ,[Value -> val*] ...) (guard (val-primitive? value-prim)) 
					(handle-operator value-prim val*)]
				[(,[Value -> val] ,[Value -> val*] ...) `(,val ,val* ...)])))
	(define Effect
		(lambda (ef)
			(match ef
				[(nop) '(nop)]
				[(if ,[Pred -> test] ,[Effect -> conseq] ,[Effect -> alt]) `(if ,test ,conseq ,alt)]
				[(begin ,[Effect -> ef*] ... ,[Effect -> ef]) `(begin ,ef* ... ,ef)]
				[(let ([,uvar* ,[Value -> value*]] ...) ,[Effect -> tail]) 
					`(let ([,uvar* ,value*] ...) ,tail)]
				[(,effect-prim ,[Value -> val*] ...) (guard (effect-primitive? effect-prim)) 
				 	(handle-effect-ops effect-prim val*)]
				[(,[Value -> val] ,[Value -> val*] ...) `(,val ,val* ...)])))
	;; Function for Immediate Primitives, we have well-defined values for #t,#f and '()
	;; For integers we shift the value by 3 bits (since we use the lower 3 bits as tag)
	(define Immediate
		(lambda (im)
			(match im
				[#t $true]
				[#f $false]
				[() $nil]
				[,t (guard (integer? t)) (ash t shift-fixnum)])))
	(define Pred
		(lambda (pred)
			(match pred
				[(true) '(true)]
				[(false) '(false)]
				[(if ,[Pred -> test] ,[Pred -> conseq] ,[Pred -> alt]) `(if ,test ,conseq ,alt)]
				[(begin ,[Effect -> ef*] ... ,[Pred -> ef]) `(begin ,ef* ... ,ef) ]
				[(let ([,uvar* ,[Value -> value*]] ...) ,[Pred -> tail]) `(let ([,uvar* ,value*] ...) ,tail)  ]
				[(,pred-prim ,[Value -> val*] ...) (guard (predicate-primitive? pred-prim)) (handle-pred-op pred-prim val*)])))
	(lambda (prog)
		(match prog
			[(letrec ([,label* (lambda (,uvar* ...) ,[Value -> val*])] ...) ,[Value -> tail]) 
				`(letrec ([,label* (lambda (,uvar* ...) ,val*)] ...) ,tail)]
			[,x (error who "invalid program ~s" x)])))


;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------

(define-who uncover-locals
  (define (Body bd)
		(define Value
			(lambda (value)
				(match value
					[(if ,[Pred -> test] ,[conseq] ,[altern]) (union test conseq altern)]
					[(begin ,[Effect -> ef*] ... ,[ef]) `(,ef* ... ... ,ef ...)]
					[(let ([,new-uvar* ,[Value -> uvar* ]] ...) ,[tail]) `(,new-uvar* ... ,tail ... ,uvar* ... ...)] ;;;return the list of new-uvar*
					[(,binop ,[Value -> x] ,[Value -> y])
					 	(guard (memq binop '(+ - * logand logor mref)))	
						(union x y)]
					[(alloc ,[Value -> val]) val]
					[(,[Value -> rator ],[Value -> rand* ] ...) `(,rator ... ,rand* ... ...)]
					[,triv '()])))
	  (define Effect
			(lambda (ef)
				(match ef
		      [(nop) '()]
		      [(if ,[Pred -> test] ,[conseq] ,[alt]) (union test conseq alt)]
					[(let ([,new-uvar* ,[Value -> uvar* ]] ...) ,[Tail -> tail]) `(,new-uvar* ... ,tail ... ,uvar* ... ...)] ;;;return the list of new-uvar*
		      [(begin ,[Effect -> ef*] ... ,[ef]) `(,ef* ... ... ,ef ...)]
					[(mset! ,[Value -> base] ,[Value -> offset] , [Value -> value]) (union base offset value)]
		      [(,[Value -> rator] ,[Value -> rand*] ...) `(,rator ... ,rand* ... ...)]
		      [,ef (error who "invalid Effect ~s" ef)])))
		(define (Pred pr)
	    (match pr
	      [(true) '()]
	      [(false) '()]
				[(let ([,new-uvar* ,[Value -> uvar* ]] ...) ,[Tail -> tail]) `(,new-uvar* ... ,tail ... ,uvar* ... ...)] ;;;return the list of new-uvar*
	      [(if ,[test] ,[conseq] ,[altern]) (union test conseq altern)]
	      [(begin ,[Effect -> ef*] ... ,[pr]) `(,ef* ... ... ,pr ...)]
	      [(,predop ,[Value -> x] ,[Value -> y])
	       (guard (memq predop '(< <= = >= >)))
					(union x y)]
	      [,pr (error who "invalid Pred ~s" pr)]))
	  (define Tail 
		 (lambda (tail)
				(match tail
		      [(if ,[Pred -> test] ,[conseq] ,[altern]) (union test conseq altern)]
		      [(begin ,[Effect -> ef*] ... ,[tail]) `(,ef* ... ... ,tail ...)]
					[(let ([,new-uvar* ,[Value -> uvar* ]] ...) ,[Value -> tail]) `(,new-uvar* ... ,tail ... ,uvar* ... ...)] ;;;return the list of new-uvar*
					[(alloc ,[Value -> val]) val]
		      [(,binop ,[Value -> x] ,[Value -> y])
		       (guard (memq binop '(+ - * logand logor sra mref)))
		      	(union x y)]
		      [(,[Value -> rator ],[Value -> rand* ] ...) `(,rator ... ,rand* ... ...)]
					[,triv '()])))
    (let ((tail (Tail bd)))
			`(locals ,tail ,bd)))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda (,uvar** ...) ,[Body -> bd*])] ...)
         ,[Body -> bd])
       `(letrec ([,label* (lambda (,uvar** ...) ,bd*)] ...) ,bd)]
      [,x (error who "invalid Program ~s" x)])))


;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------

(define-who remove-let
  (define (Body bd)
		(define Value
			(lambda (value)
				(match value
					[(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
					[(begin ,[Effect -> ef*] ... ,[ef]) (make-begin `(,ef* ...,ef))]
					[(let ([,new-uvar* ,[Value -> uvar* ]] ...) ,[Value -> tail]) 
							(let ((begin-exps (map (lambda (uvar exp) 
																					`(set! ,uvar ,exp)) new-uvar* uvar*)))
										(make-begin `(,begin-exps ...,tail)))]
					[(,binop ,[Value -> x] ,[Value -> y])
					 	(guard (memq binop '(+ - * logand logor mref)))	
						`(,binop ,x ,y)]
					[(alloc ,[Value -> val]) `(alloc ,val)]
					[(,[Value -> rator ],[Value -> rand* ] ...) `(,rator ,rand* ...)]
					[,triv triv])))
	  (define Effect
			(lambda (ef)
				(match ef
		      [(nop) '(nop)]
		      [(if ,[Pred -> test] ,[conseq] ,[alt]) `(if ,test ,conseq ,alt)]
					[(let ([,new-uvar* ,[Value -> uvar* ]] ...) ,[Value -> tail]) 
							(let ((begin-exps (map (lambda (uvar exp) 
																					`(set! ,uvar ,exp)) new-uvar* uvar*)))
										(make-begin `(,begin-exps ...,tail)))]
					[(begin ,[Effect -> ef*] ... ,[ef]) (make-begin `(,ef* ... ,ef))]
					[(mset! ,[Value -> base] ,[Value -> offset] , [Value -> value]) `(mset! ,base ,offset ,value)]
		      [(,[Value -> rator ],[Value -> rand* ] ...) `(,rator ,rand* ...)]
		      [,ef (error who "invalid Effect ~s" ef)])))
		(define (Pred pr)
	    (match pr
	      [(true) '(true)]
	      [(false) '(false)]
				[(let ([,new-uvar* ,[Value -> uvar* ]] ...) ,[Value -> tail]) 
						(let ((begin-exps (map (lambda (uvar exp) 
																				`(set! ,uvar ,exp)) new-uvar* uvar*)))
									(make-begin `(,begin-exps ...,tail)))]
	      [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
	      [(begin ,[Effect -> ef*] ... ,[pr]) (make-begin `(,ef* ... ,pr))]
	      [(,predop ,[Value -> x] ,[Value -> y])
	       (guard (memq predop '(< <= = >= >)))
					`(,predop ,x ,y)]
	      [,pr (error who "invalid Pred ~s" pr)]))
	  (define Tail 
		 (lambda (tail)
				(match tail
		      [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
		      [(begin ,[Effect -> ef*] ... ,[tail]) (make-begin `(,ef* ... ,tail))]
					[(let ([,new-uvar* ,[Value -> uvar* ]] ...) ,[Value -> tail]) 
							(let ((begin-exps (map (lambda (uvar exp) 
																					`(set! ,uvar ,exp)) new-uvar* uvar*)))
										(make-begin `(,begin-exps ...,tail)))]
					[(alloc ,[Value -> val]) `(alloc ,val)]
		      [(,binop ,[Value -> x] ,[Value -> y])
		       (guard (memq binop '(+ - * logand logor sra mref)))
		      	`(,binop ,x ,y)]
		      [(,[Value -> rator ],[Value -> rand* ] ...) `(,rator ,rand* ...)]
					[,triv triv])))
   (match bd
			[(locals (,uvar* ...) ,[Tail -> tail]) `(locals (,uvar* ...) ,tail)]))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda (,uvar** ...) ,[Body -> bd*])] ...)
         ,[Body -> bd])
       `(letrec ([,label* (lambda (,uvar** ...) ,bd*)] ...) ,bd)]
      [,x (error who "invalid Program ~s" x)])))


;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------

(define-who verify-uil
  (define verify-x-list
    (lambda (x* x? what)
      (let loop ([x* x*] [idx* '()])
        (unless (null? x*)
          (let ([x (car x*)] [x* (cdr x*)])
            (unless (x? x)
              (error who "invalid ~s ~s found" what x))
            (let ([idx (extract-suffix x)])
              (when (member idx idx*)
                (error who "non-unique ~s suffix ~s found" what idx))
              (loop x* (cons idx idx*))))))))
  (define Triv
    (lambda (label* uvar*)
      (lambda (t)
        (unless (or (label? t) (uvar? t) (and (integer? t) (exact? t)))
          (error who "invalid Triv ~s" t))
        (when (and (integer? t) (exact? t))
          (unless (int64? t)
            (error who "integer out of 64-bit range ~s" t)))
        (when (uvar? t)
          (unless (memq t uvar*)
            (error who "reference to unbound uvar ~s" t)))
        (when (label? t)
          (unless (memq t label*)
            (error who "unbound label ~s" t)))
        t)))
  (define Value
    (lambda (label* uvar*)
      (lambda (val)
        (match val
          [(if ,[(Pred label* uvar*) -> test] ,[conseq] ,[altern]) (void)]
          [(begin ,[(Effect label* uvar*) -> ef*] ... ,[val]) (void)]
					[(alloc ,[(Value label* uvar*) -> mem-size]) (void)]	;;clause for alloc
					[(mref ,[(Value label* uvar*) -> base] ,[(Value label* uvar*) -> offset]) (void)] ;;clause for mref
          [(sra ,[x] ,y)
           (unless (uint6? y)
             (error who "invalid sra operand ~s" y))]
          [(,binop ,[x] ,[y])
           (guard (memq binop '(+ - * logand logor sra)))
           (void)]
          [(,[rator] ,[rand*] ...) (void)]
          [,[(Triv label* uvar*) -> tr] (void)]))))
  (define Effect
    (lambda (label* uvar*)
      (lambda (ef)
        (match ef
          [(nop) (void)]
          [(if ,[(Pred label* uvar*) -> test] ,[conseq] ,[altern]) (void)]
          [(begin ,[ef*] ... ,[ef]) (void)]
          [(set! ,var ,[(Value label* uvar*) -> val])
           (unless (memq var uvar*)
             (error who "assignment to unbound var ~s" var))]
					[(mset! ,[(Value label* uvar*) -> base] ,[(Value label* uvar*) -> offset] ,[(Value label* uvar*) -> val]) (void)] ;;clause for mset!
          [(,[(Value label* uvar*) -> rator] 
             ,[(Value label* uvar*) -> rand*] ...)
           (void)]
          [,ef (error who "invalid Effect ~s" ef)]))))
  (define Pred
    (lambda (label* uvar*)
      (lambda (pr)
        (match pr
          [(true) (void)]
          [(false) (void)]
          [(if ,[test] ,[conseq] ,[altern]) (void)]
          [(begin ,[(Effect label* uvar*) -> ef*] ... ,[pr]) (void)]
          [(,predop ,[(Value label* uvar*) -> x] ,[(Value label* uvar*) -> y])
           (unless (memq predop '(< > <= >= =))
             (error who "invalid predicate operator ~s" predop))]
          [,pr (error who "invalid Pred ~s" pr)]))))
  (define Tail
    (lambda (tail label* uvar*)
      (match tail
        [(if ,[(Pred label* uvar*) -> test] ,[conseq] ,[altern]) (void)]
        [(begin ,[(Effect label* uvar*) -> ef*] ... ,[tail]) (void)]
				[(alloc ,[(Value label* uvar*) -> mem-size]) (void)]			;;clause for alloc
				[(mref ,[(Value label* uvar*) -> base] ,[(Value label* uvar*) -> offset]) (void)] ;;clause for mref
        [(sra ,[(Value label* uvar*) -> x] ,y)
         (unless (uint6? y)
           (error who "invalid sra operand ~s" y))]
        [(,binop ,[(Value label* uvar*) -> x] ,[(Value label* uvar*) -> y])
         (guard (memq binop '(+ - * logand logor sra)))
         (void)]
        [(,[(Value label* uvar*) -> rator] 
           ,[(Value label* uvar*) -> rand*] ...)
         (void)]
        [,[(Triv label* uvar*) -> triv] (void)])))
  (define Body
    (lambda (label*)
      (lambda (bd fml*)
        (match bd
          [(locals (,local* ...) ,tail)
           (let ([uvar* `(,fml* ... ,local* ...)])
             (verify-x-list uvar* uvar? 'uvar)
             (Tail tail label* uvar*))]
          [,bd (error who "invalid Body ~s" bd)]))))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda (,fml** ...) ,bd*)] ...) ,bd)
       (verify-x-list label* label? 'label)
       (map (lambda (fml*) (verify-x-list fml* uvar? 'formal)) fml**)
       (for-each (Body label*) bd* fml**)
       ((Body label*) bd '())]
      [,x (error who "invalid Program ~s" x)])
    x))


;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------

;;;When I encounter any function calls in Value and effect contexts it simply calls trivialize calls, thats the only change from last weeks assignment
;;;To deal with new forms introduced by this weeks grammar I am trivializing the arguments to all the mref mset! and alloc 
;;; eg (set! x.1 (alloc (begin (set! y.2 16) (set! z.3 32) (+ y.2 z.3)))) wiil get translated to
;;;(begin
;;;(set! y.2 16)
;;;(set! z.3 32)
;;;(set! t.4 (+ y.2 z.3))
;;;(set! x.1 (alloc t.4)))
;;;It will be similar if we encounter mrefs and mset! with complex arguments
;;;I am treating an mref as an ordinary binop for this pass as it produces the exactly same output as any other binop
;;;clauses are added for mset! in Effect and alloc in tail and value contexts

(define-who remove-complex-opera*
  (define (Body bd)
    (define new-local* '())
    (define (new-t)
      (let ([t (unique-name 't)])
        (set! new-local* (cons t new-local*))
        t))
    (define (trivialize-call expr*)
      (let-values ([(call set*) (break-down-expr* expr*)])
        (make-begin `(,@set* ,call))))
    (define break-down-expr*
			(lambda (expr*)
				(match expr*
	        [() (values '() '())]
					[(alloc . ,[break-down-expr* -> rest* set*])
						(values `(alloc ,rest* ...) set*)]
					[(mset! . ,[break-down-expr* -> rest* set*])
						(values `(mset! ,rest* ...) set*)]
	        [(,s . ,[break-down-expr* -> rest* set*]) 
	         (guard (simple? s)) 
	         (values `(,s ,rest* ...) set*)]
	        [(,[Value -> expr] . , [break-down-expr* -> rest* set*])
	         (let ([t (new-t)]) 
	           (values `(,t ,rest* ...) `((set! ,t ,expr) ,set* ...)))]
	        [,expr* (error who "invalid Expr ~s" expr*)])))
    (define (simple? x)
      (or (uvar? x) (label? x) (and (integer? x) (exact? x))
          (memq x '(+ - * logand logor sra mref)) (memq x '(= < <= > >=))))
    (define Value
			(lambda (val)
				(match val
	        [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
	        [(begin ,[Effect -> ef*] ... ,[val]) (make-begin `(,ef* ... ,val))]
					[(alloc ,[Value -> val]) (trivialize-call `(alloc ,val))]
					[(,binop ,[Value -> x] ,[ Value -> y])
	         (guard (memq binop '(+ - * logand logor sra mref)))
	         (trivialize-call `(,binop ,x ,y))]
	        [(,rator ,rand* ...) (trivialize-call `(,rator ,rand* ...))]
	        [,tr tr])))
    (define (Effect ef)
      (match ef
        [(nop) '(nop)]
				[(mset! ,[Value -> val1] ,[Value -> val2] ,[Value -> val3]) (trivialize-call `(mset! ,val1 ,val2 ,val3))]
        [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
        [(begin ,[ef*] ... ,[ef]) (make-begin `(,ef* ... ,ef))]
        [(set! ,var ,[Value -> val]) `(set! ,var ,val)]
        [(,rator ,rand* ...) (trivialize-call `(,rator ,rand* ...))]
        [,ef (error who "invalid Effect ~s" ef)]))
    (define (Pred pr)
      (match pr
        [(true) '(true)]
        [(false) '(false)]
        [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
        [(begin ,[Effect -> ef*] ... ,[pr]) (make-begin `(,ef* ... ,pr))]
        [(,predop ,x ,y)
         (guard (memq predop '(< <= = >= >)))
         (trivialize-call `(,predop ,x ,y))]
        [,pr (error who "invalid Pred ~s" pr)]))
    (define (Tail tail)
      (match tail
        [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
        [(begin ,[Effect -> ef*] ... ,[tail]) (make-begin `(,ef* ... ,tail))]
				[(alloc ,[Value -> val]) (trivialize-call `(alloc ,val))]
        [(,binop ,[Value -> x] ,[Value -> y])
         (guard (memq binop '(+ - * logand logor sra mref)))
         (trivialize-call `(,binop ,x ,y))]
        [(,rator ,rand* ...) (trivialize-call `(,rator ,rand* ...))]
        [,tr tr]))
    (match bd
      [(locals (,local* ...) ,[Tail -> tail])
       `(locals (,local* ... ,new-local* ...) ,tail)]
      [,bd (error who "invalid Body ~s" bd)]))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda (,fml** ...) ,[Body -> bd*])] ...) 
         ,[Body -> bd])
       `(letrec ([,label* (lambda (,fml** ...) ,bd*)] ...) ,bd)]
      [,x (error who "invalid Program ~s" x)])))


;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;; I decided to get rid of the alloc forms in this pass by introducing the allocaton pointer register;
;;; There are 2 essential places where the alloc form will be encountered
;;; a) tail context and b) Value context
;;; if i encounter alloc in tail context e.g (alloc 48)
;;; all I do is convert it into 3 statements (alloc 48) => (begin (set! temp.1 ap) (set! ap (+ ap 48)) temp.1)
;;; if i encounter alloc in value context e.g (set! x.2 (alloc 48))
;;; all I do is convert it into 2 statements (set! x.2 (alloc 48)) => (begin (set! x.2 ap) (set! ap (+ ap 48)))
;;; mrefs continue to be treated as binops
;;; msets are allowed to pass on as it is
 
(define-who flatten-set!
  (define (Body bd)
		(define trivialize-set!
			(lambda (lhs rhs)
				(match rhs
		      [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
		      [(begin ,[Effect -> ef*] ... ,[tail]) (make-begin `(,ef* ... ,tail))]
					[(alloc ,val1) (make-begin 
													 `((set! ,lhs ,allocation-pointer-register) 
														(set! ,allocation-pointer-register (+ ,allocation-pointer-register ,val1))))]
		      [(,binop ,x ,y) 
		       (guard (memq binop '(+ - * logand logor sra mref)))
		       `(set! ,lhs (,binop ,x ,y))]
		      [(,rator ,rand* ...) `(set! ,lhs (,rator ,rand* ...))] ;This will make it (set! t.1 (ack$0 2 3)) and push the expression to the end
		      [,tr `(set! ,lhs ,tr)]))) ;;treating mref's as binop
	  (define Effect
			(lambda (ef)
				(match ef
		      [(nop) '(nop)]
		      [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
		      [(begin ,[Effect -> ef*] ... ,[ef]) (make-begin `(,ef* ... ,ef))]
					[(mset! ,base ,offset ,value) `(mset! ,base ,offset ,value)]
		      [(set! ,var ,val) (trivialize-set! var val)]
		      [(,rator ,rand* ...) `(,rator ,rand* ...)]
		      [,ef (error who "invalid Effect ~s" ef)])))
		(define (Pred pr)
	    (match pr
	      [(true) '(true)]
	      [(false) '(false)]
	      [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
	      [(begin ,[Effect -> ef*] ... ,[pr]) (make-begin `(,ef* ... ,pr))]
	      [(,predop ,x ,y)
	       (guard (memq predop '(< <= = >= >)))
	       `(,predop ,x ,y)]
	      [,pr (error who "invalid Pred ~s" pr)]))
	  (define (Tail tail) 
			(match tail
	      [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
	      [(begin ,[Effect -> ef*] ... ,[tail]) (make-begin `(,ef* ... ,tail))]
				[(alloc ,val)
					(let ((new-temp (new-t)))
						(make-begin `((set! ,new-temp ,allocation-pointer-register) 
													(set! ,allocation-pointer-register (+ ,allocation-pointer-register ,val))
													,new-temp)))]
	      [(,binop ,x ,y)
	       (guard (memq binop '(+ - * logand logor sra mref)))
	      `(,binop ,x ,y)]
	      [(,rator ,rand* ...) `(,rator ,rand* ...)]
	      [,tr tr]))
		(define new-local* '())
	  (define (new-t)
	    (let ([t (unique-name 't)])
	      (set! new-local* (cons t new-local*))
	      t))
    (match bd
      [(locals (,uvar* ...) ,[Tail -> tail]) `(locals (,uvar* ...,new-local* ...) ,tail)]
      [,bd (error who "invalid Body ~s" bd)]))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda (,fml** ...) ,[Body -> bd*])] ...)
         ,[Body -> bd])
       `(letrec ([,label* (lambda (,fml** ...) ,bd*)] ...) ,bd)]
      [,x (error who "invalid Program ~s" x)])))

;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------

;;;Program	----->	(letrec ([label (lambda (uvar*) Body)]*) Body)
;;;Body	----->	(locals (uvar*) Tail)
;;;Tail	----->	Triv
;;;	|	(binop Triv Triv)
;;;	|	(Triv Triv*)
;;; | (mref Triv Triv)
;;;	|	(if Pred Tail Tail)
;;;	|	(begin Effect* Tail)
;;;Pred	----->	(true)
;;;	|	(false)
;;;	|	(relop Triv Triv)
;;;	|	(if Pred Pred Pred)
;;;	|	(begin Effect* Pred)
;;;Effect	----->	(nop)
;;;	|	(set! uvar Triv)
;;;	|	(set! uvar (binop Triv Triv))
;;;	|	(set! uvar (Triv Triv*))
;;; | (mset! Triv Triv Triv)
;;;	|	(Triv Triv*)
;;;	|	(if Pred Effect Effect)
;;;	|	(begin Effect* Effect)
;;;Triv	----->	uvar | int | label

;;;Now That I have got rid of the alloc forms impose calling conventions is trivial....
;;;mref continues to be treated as binop and mset! expressions are passed on as they are
;;; Though only addition is that the allocation pointer register is added here to the list of registers 
;;; that are live on entry...This helps in allocating registers properly to conflicting variables, otherwise me may
;;;happen to override the alocation pointer regster and access wrong regions in the memory

(define-who impose-calling-conventions
  (define (argument-locations fmls idx->fv)
    (let f ([fmls fmls] [regs parameter-registers] [fv-idx 0])
      (cond
        [(null? fmls) '()]
        [(null? regs) (cons (idx->fv fv-idx) (f (cdr fmls) regs (+ fv-idx 1)))]
        [else (cons (car regs) (f (cdr fmls) (cdr regs) fv-idx))])))
  (define (index->new-frame-var idx) (unique-name 'nfv))
	;;Filters  list for values based on the predicate passed by fn
	(define filter 			
		(lambda (fn ls)	
			(cond
				[(null? ls) '()]
				[(fn (car ls)) (cons (car ls) (filter fn (cdr ls)))]
				[else (filter fn (cdr ls))])))
	(define trivial?
		(lambda (x)
			(or (uvar? x) (integer? x) (label? x) (register? x))))
  (define (Body bd fml*)
   	(define new-frame-var** '())  ;;Stores all the nfv assignments
    (define Effect
			(lambda (effect)
				(match effect
					[(nop) '(nop)]
					[(if ,[Pred -> pred] ,[Effect -> conseq] ,[Effect -> alt]) `(if ,pred ,conseq ,alt)]
					[(begin ,[Effect -> ef*] ... ,[Effect -> ef]) (make-begin `(,ef* ... ,ef))]
					[(mset! ,base ,offset ,val) effect]
					[(set! ,uvar ,triv) (guard (trivial? triv)) effect]					
					[(set! ,uvar (,binop ,x ,y)) (guard (memq binop '(+ - * logand logor sra mref))) effect]
					[(set! ,uvar (,triv ,triv* ...)) 
							(guard (trivial? triv))
										(make-begin `(,(Effect `(,triv ,triv* ...)) (set! ,uvar ,return-value-register)))]												
					[(,triv ,triv* ...)	
							;This handles non tail call in Effect Context
							(let* ((return-point-var (unique-label 'rp))
										 (fml-loc* (argument-locations triv* index->new-frame-var)) ;Assign a register or variable to each formal parameter
								 		 (expr (make-begin 
											`((set! ,fml-loc* ,triv*) ... (set! ,return-address-register ,return-point-var) (,triv ,return-address-register ,frame-pointer-register ,allocation-pointer-register ,@fml-loc*))))
										(return-point-expr `(return-point ,return-point-var ,expr))) ;Create a return-point-expr
										(set! new-frame-var** (cons (filter uvar? fml-loc*) new-frame-var**))
										(make-begin `(,return-point-expr)))])))
    (define (Pred pred) 
			(match pred
				[(true) '(true)]
        [(false) '(false)]
        [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
        [(begin ,[Effect -> ef*] ... ,[pr]) (make-begin `(,ef* ... ,pr))]
        [(,predop ,x ,y)
         (guard (memq predop '(< <= = >= >)))
         `(,predop ,x ,y)]))
    (define Tail
			(lambda (tail rp)
				(match tail
					[(begin ,[Effect -> ef*] ... ,tail) 
					(let ((tail-expr (Tail tail rp)))
									(make-begin `(,ef* ... ,tail-expr)))]
					[(if ,[Pred -> pred] ,conseq ,alt)
					 	(let ((conseq-expr (Tail conseq rp))
									(alt-expr (Tail alt rp)))
									`(if ,pred ,conseq-expr ,alt-expr))]
					[(,binop ,x ,y) (guard (memq binop '(+ - * logand logor sra mref)))
					 		(let ((expr `((set! ,return-value-register ,tail) (,rp ,frame-pointer-register ,allocation-pointer-register ,return-value-register))))
								(make-begin expr))]
					[(,triv ,triv* ...) 
							(let ((fml-loc* (reverse (argument-locations triv* index->frame-var))) (triv* (reverse triv*)))
								(make-begin 
									`((set! ,fml-loc* ,triv*) ... (set! ,return-address-register ,rp) (,triv ,return-address-register ,frame-pointer-register ,allocation-pointer-register ,@fml-loc*))))]
					[,triv 	(let ((return-value-expr `(set! ,return-value-register ,triv))
							   				(return-calling-expr `(,rp ,frame-pointer-register ,allocation-pointer-register ,return-value-register)))
									(make-begin `(,return-value-expr ,return-calling-expr)))])))
    (match bd
      [(locals (,local* ...) ,tail)
       (let ([rp (unique-name 'rp)]
             [fml-loc* (argument-locations fml* index->frame-var)])
         (let ([tail (Tail tail rp)])
           `(locals (,rp ,fml* ... ,local* ... ,new-frame-var** ... ...)
              (new-frames (,new-frame-var** ...)
                ,(make-begin 
                   `((set! ,rp ,return-address-register)
                     (set! ,fml* ,fml-loc*) ...
                     ,tail))))))]
      [,bd (error who "invalid Body ~s" bd)]))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda (,fml** ...) ,bd*)] ...) ,bd)
       (let ([bd* (map Body bd* fml**)] [bd (Body bd '())])
         `(letrec ([,label* (lambda () ,bd*)] ...) ,bd))]
      [,x (error who "invalid Program ~s" x)])))


;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------

;;;Program	----->	(letrec ([label (lambda () Body)]*) Body)
;;;Body	----->	(locals (uvar*)
;;;		  (new-frames (Frame*) Tail))
;;;Frame	----->	(uvar*)
;;;Tail	----->	(Triv Loc*)
;;;	|	(if Pred Tail Tail)
;;;	|	(begin Effect* Tail)
;;;Pred	----->	(true)
;;;	|	(false)
;;;	|	(relop Triv Triv)
;;;	|	(if Pred Pred Pred)
;;;	|	(begin Effect* Pred)
;;;Effect	----->	(nop)
;;;	|	(set! Var Triv)
;;;	|	(set! Var (binop Triv Triv))
;;;	|	(return-point label (Triv Loc*))
;;;	|	(if Pred Effect Effect)
;;;	|	(begin Effect* Effect)
;;;Loc	----->	reg | fvar
;;;Var	----->	uvar | Loc
;;;Triv	----->	Var | int | label

;;;uncover frame conflicts needs to handle mrefs and mset's in different way while updating the conflict table
;;;(set! x (mref base offset)) => will mean that x conflicts with both base and offset and hence we must record these conflicts
;;;(mset! base offset value) => in this we have to record the conflicts of each parameters with the other
 
(define-who uncover-frame-conflict
  (define add-conflicts!
	    (lambda (ct lhs live*)
	      (define add-conflict!
	        (lambda (var1 var2)
	          (let ([a (assq var1 ct)])
	            (set-cdr! a (if (eq? var1 var2) (cdr a)  (set-cons var2 (cdr a)))))))
	      (when (uvar? lhs)
	        (for-each
	          (lambda (live) (add-conflict! lhs live))
	          live*))
	      (for-each
	        (lambda (live) (when (and (uvar? live) (not (register? lhs))) (add-conflict! live lhs)))
	        live*)))
	(define trivial?
		(lambda (x)
			(or (uvar? x) (integer? x) (label? x))))
	(define remove-nulls
		(lambda (ls)
			(cond
				[(null? ls) '()]
				[(null? (car ls)) (remove-nulls (cdr ls))]
				[else (set-cons (car ls) (remove-nulls (cdr ls)))])))
  (define (Body x)
    (define call-live* '())
    (define Triv (lambda (x) (if (or (uvar? x) (frame-var? x)) `(,x) '())))
    (define Effect*
      (lambda (x live* ct)
        (match x
          [() live*]
          [(,ef* ... ,ef) (Effect* ef* (Effect ef live* ct) ct)]
          [,x (error who "invalid Effect* list ~s" x)])))
    (define Effect
      (lambda (x live* ct)
        (match x
          [(nop) live*]
          [(if ,test ,[c-live*] ,[a-live*]) 
					 	(Pred test  c-live* a-live* ct)]
          [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
					[(mset! ,[Triv -> base] ,[Triv -> offset] ,[Triv -> value])
						(begin
												(if (not (null? base)) (add-conflicts! ct (car base) (union offset value live*)))
																	(if (not (null? offset)) (add-conflicts! ct (car offset) (union base value live*)))
																	(if (not (null? value)) (add-conflicts! ct (car value) (union offset base live*)))
																	(union base offset value live*))]
					[(set! ,lhs (mref ,[Triv -> x-live*] ,[Triv -> y-live*]))
					           (begin
												(add-conflicts! ct lhs (union x-live* y-live* live*))
												(union x-live* y-live* (remq lhs live*)))]
          [(set! ,lhs (,binop ,[Triv -> x-live*] ,[Triv -> y-live*]))
					 (guard (memq binop '(+ - * logand logor)))	
           (begin
							(add-conflicts! ct lhs live*)
							(union x-live* y-live* (remq lhs live*)))]
          [(set! ,lhs ,var)
							(begin
								(add-conflicts! ct lhs live*)
								(if (or (uvar? var) (frame-var? var)) (set-cons var (remq lhs live*)) (remq lhs live*)))]
          [(return-point ,rplab ,tail)
						(let ((new-live* (Tail tail ct)))
								(set! call-live* (union call-live* live*))
								           (union live* new-live*))]
          [,x (error who "invalid Effect list ~s" x)])))
    (define Pred
      (lambda (x t-live* f-live* ct)
        (match x
          [(true) t-live* ]
          [(false) f-live* ]
          [(if ,test ,[c-live*] ,[a-live*]) 
						(union t-live* f-live* (Pred test c-live* a-live* ct))]
          [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
          [(,predop ,[Triv -> x-live*] ,[Triv -> y-live*])
           (remove-nulls (union x-live* y-live* t-live* f-live*))]
          [,x (error who "invalid Pred ~s" x)])))
    (define Tail
      (lambda (x ct)
        (match x
          [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
          [(if ,test ,[c-live*] ,[a-live*]) (Pred test c-live* a-live* ct)]
          [(,[Triv -> target] ,[Triv -> live*] ...) `(,target ... ,live* ... ...)]
          [,x (error who "invalid Tail ~s" x)])))
    (match x
      [(locals (,uvar* ...) (new-frames (,nfv** ...) ,tail))
       (let ([ct (map (lambda (x) (cons x '())) uvar*)])
         (let ([uvar* (filter uvar? (Tail tail ct))])
           (unless (null? uvar*)
             (error who "found variables ~s live on entry" uvar*)))
         (let ([spill* (filter uvar? call-live*)])
           `(locals (,(difference uvar* spill*) ...)
               (new-frames (,nfv** ...)
                 (spills ,spill*
                   (frame-conflict ,ct
                     (call-live (,call-live* ...) ,tail)))))))]
      [,x (error who "invalid Body ~s" x)]))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,x (error who "invalid Program ~s" x)])))


;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------

;Simpler version of assign frame assigns a frame variable for all variables in the spill list, done through the find-homes function

(define-who pre-assign-frame
		(define replace																		;;Replaces the occurences of variables in the conflict-list with the register-homes
			(lambda (allocations ct)
				(cond 
					[(null? allocations) ct]
					[else (replace (cdr allocations) (replace-helper (car allocations) ct))])))
		(define replace-helper
			(lambda (allocation ct)
				(map (lambda (ct-entry)
								(cond
									[(eq? (car allocation) (car ct-entry)) allocation]
									[else (cons (car ct-entry) (replace* (cdr ct-entry) allocation))])) ct)))
		(define replace*
			(lambda (conflicts allocation)
				(cond
					[(null? conflicts) '()]
					[(eq? (car conflicts) (car allocation)) (cons (cadr allocation) (replace* (cdr conflicts) allocation))]
					[else (cons (car conflicts) (replace* (cdr conflicts) allocation))])))			
		(define update-bias-table
			(lambda (uvar reg bt)
				(map (lambda (entry) 
								(let ([tail (cdr entry)])
									(if (memq uvar tail)
											(begin
												(set-cdr! entry (cons reg (remq uvar entry)))
												entry)
												entry))) bt)))
		(define update-conflict-table
			(lambda (uvar reg ct)
				(map (lambda (entry) 
								(let ([tail (cdr entry)])
									(if (memq uvar tail)
											(begin
												(set-cdr! entry (cons reg (remq uvar entry)))
												entry)
												entry))) ct)))
	(define assign-homes
		(lambda (var* bt ct)
			(cond
				[(null? bt) '() ]
				[(null? var*) '()]
				[else (let* ([current-var (car var*)]
										 [bt-entry (assq current-var bt)]
										 [friend-list (cdr bt-entry)]
										 [conflict-entry (cdr (assq current-var ct))])
												(if (null? friend-list) 
														(assign-homes (cdr var*) bt ct)
														(let ([friend-frame (get-frame friend-list conflict-entry)])
															(cond
																[(eq? friend-frame #f) (assign-homes (cdr var*) bt ct)]
																[else (let ([updated-ct (update-conflict-table current-var friend-frame ct)]
																						[updated-bt (update-bias-table current-var friend-frame bt)])
																						(cons (list current-var friend-frame) (assign-homes (cdr var*) updated-bt updated-ct)))]))))])))
	(define get-frame
		(lambda (ls conflict-entry)
			(cond
				[(null? ls) #f]
				[(and (frame-var? (car ls)) (not (memq (car ls) conflict-entry))) (car ls)]
				[else (get-frame (cdr ls) conflict-entry)])))
	(define find-used
	    (lambda (conflict* home*)
	      (cond
	        [(null? conflict*) '()]
	        [(frame-var? (car conflict*)) 
	         (set-cons (car conflict*) (find-used (cdr conflict*) home*))]
	        [(assq (car conflict*) home*) => 
	         (lambda (x) (set-cons (cadr x) (find-used (cdr conflict*) home*)))]
	        [else (find-used (cdr conflict*) home*)])))
	  (define find-frame-var
	    (lambda (used*)
	      (let f ([index 0])
	        (let ([fv (index->frame-var index)])
	          (if (memq fv used*) (f (+ index 1)) fv)))))
	  (define find-homes
	    (lambda (var* ct home*)
	      (if (null? var*)
	          home*
	          (let ([var (car var*)] [var* (cdr var*)])
	            (let ([conflict* (cdr (assq var ct))])
	              (let ([home (find-frame-var (find-used conflict* home*))])
	                (find-homes var* ct `((,var ,home) . ,home*))))))))
  (define Body
    (lambda (body)
      (match body
        [(locals (,local* ...)
           (new-frames (,nfv** ...)
             (spills (,spill* ...)
               (frame-conflict ,ct
                 (call-live (,call-live* ...) ,tail)))))
         (let* ([uvar* (union local* spill*)]
								[bias-list (move-bias tail frame-var? uvar*)]
								[biased-home* (assign-homes spill* bias-list ct)]
								[home (if (null? biased-home*) '() (map car biased-home*))]
							  [new-ct (if (null? biased-home*) ct (replace biased-home* ct))]
							  [home* (find-homes (difference spill* home) ct '())])
           `(locals (,local* ...)
              (new-frames (,nfv** ...)
                (locate (,biased-home* ... ,home* ...)
                  (frame-conflict ,ct
                    (call-live (,call-live* ...) ,tail))))))]
        [,body (error who "invalid Body ~s" body)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,x (error who "invalid Program ~s" x)])))


;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------


(define-who move-bias
	(define uncover-bias
		(lambda (body bias-list fn)
			(match body
				[(begin ,ef* ...) (Effect* ef* bias-list fn)])))
	(define Effect*
		(lambda (effect* bias-list fn)
			(match effect*
				[,x (guard (null? x)) bias-list]
				[(,ef,ef* ...) (Effect* ef* (Effect ef bias-list fn) fn)])))
	(define Effect
		(lambda (ef bias-list fn)
			(match ef
				[(set! ,x ,y) (guard (and (uvar? x) (uvar? y)))
				 							(begin
												(set-cdr! (assq x bias-list) (set-cons y (cdr (assq x bias-list))))
												(set-cdr! (assq y bias-list) (set-cons x (cdr (assq y bias-list))))
												bias-list)]
				[(set! ,x ,y) (guard (and (uvar? x) (fn y)))
											  (begin
													(set-cdr! (assq x bias-list) (set-cons y (cdr (assq x bias-list))))
													bias-list)]
				[(set! ,x ,y) (guard (and (fn x) (uvar? y)))
											  (begin
													(set-cdr! (assq y bias-list) (set-cons x (cdr (assq y bias-list))))
													bias-list)]
				[,x bias-list])))
	(lambda (tail fn uvar*)
		(cond
			[(eq? move-bias-enabled '#f) '()]
			[else (let* ([bias-list (map (lambda (x) (cons x '())) uvar*)]
						 			 [new-bias-list (uncover-bias tail bias-list fn)])
									 new-bias-list)])))

;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------

;;;(return-point rp-label tail) =>
;;;  (begin
;;;    (set! fp (+ fp nb))
;;;    (return-point rp-label tail)
;;;    (set! fp (- fp nb)))

(define-who assign-new-frame
  (define Effect
    (lambda (fs)
      (lambda (x)
        (match x
          [(nop) '(nop)]
          [(if ,[(Pred fs) -> test] ,[(Effect fs) -> conseq] ,[(Effect fs) -> altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[ef*] ... ,[ef]) 	(make-begin `(,ef* ... ,ef))]
					[(mset! ,base ,offset ,value) `(mset! ,base ,offset ,value)]
          [(set! ,lhs ,rhs) `(set! ,lhs ,rhs)]
          [(return-point ,rplab ,[(Tail fs) -> tail])
							(make-begin
								`((set! ,frame-pointer-register (+ ,frame-pointer-register ,(ash fs align-shift)))
									,x
									(set! ,frame-pointer-register (- ,frame-pointer-register ,(ash fs align-shift)))))]
          [,x (error who "invalid Effect ~s" x)]))))
  (define Pred
    (lambda (fs)
      (lambda (x)
        (match x
          [(true) '(true)]
          [(false) '(false)]
          [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
          [(begin ,[(Effect fs) -> ef*] ... ,[pr])  (make-begin `(,ef* ... ,pr))]
          [(,predop ,x ,y) `(,predop ,x ,y)]
          [,x (error who "invalid Pred ~s" x)]))))
  (define Tail
    (lambda (fs)
      (lambda (x)
        (match x
          [(if ,[(Pred fs) -> test] ,[(Tail fs) -> conseq] ,[ (Tail fs) -> altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[(Effect fs) -> ef*] ... ,[(Tail fs) -> tail]) (make-begin `(,ef* ... ,tail))]
          [(,triv ,live* ...) `(,triv ,live* ...)]
          [,x (error who "invalid Tail ~s" x)]))))
	(define find-max					;;This function is used to determine the size of the frame used by the function
		(lambda (ls)
			(cond
				[(null? ls) '-1 ]
				[else (max (car ls) (find-max (cdr ls)))])))
	;;; The function argument to map does all the work we basically have to find the max-index of all call-live variables which can either be a frame variable
	;;; or we could look up the frame variable assigned to a uvar via the pre-assign frame pass
  (define Body
    (lambda (x)
      (define frame-size 
				(lambda (call-live* home*)
					(let ([ls (map (lambda (x)
														(if (frame-var? x) 
																(frame-var->index x)
																(frame-var->index (cadr (assq x home*))))) call-live*)])
								(add1 (find-max ls)))))
      (match x
        [(locals (,local* ...)
           (new-frames (,nfv** ...)
             (locate (,home* ...)
               (frame-conflict ,ct
                 (call-live (,call-live* ...) ,tail)))))
         (let ([fs (frame-size call-live* home*)])
           (define (do-assign var*)
             (let f ([index fs] [ls var*] [rs '()])
								(let ((fv (index->frame-var index)))
									(cond
										[(null? ls) rs]
										[else (f (add1 index) (cdr ls) (cons `(,(car ls) ,fv) rs))]))))
           `(locals (,(difference local* `(,nfv** ... ...)) ...)
              (ulocals ()
                (locate (,home* ... ,(map do-assign nfv**) ... ...)
                  (frame-conflict ,ct ,((Tail fs) tail))))))]
        [,x (error who "invalid Body ~s" x)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,x (error who "invalid Program ~s" x)])))


;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------

;;; mrefs are still handled as binops here
;;; mset! clause added, if any of the parameters has been assigned as a frame it is reflected here

(define-who finalize-frame-locations
  (define Var
    (lambda (env)
      (lambda (v)
        (cond
          [(and (uvar? v) (assq v env)) => cdr]
          [else v]))))
  (define Triv Var)
  (define Pred
    (lambda (env)
      (lambda (pr)
        (match pr
          [(true) '(true)]
          [(false) '(false)]
          [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
          [(begin ,[(Effect env) -> ef*] ... ,[pr]) `(begin ,ef* ... ,pr)]
          [(,predop ,[(Triv env) -> x] ,[(Triv env) -> y]) `(,predop ,x ,y)]
          [,pr (error who "invalid Pred ~s" pr)]))))
  (define Effect
    (lambda (env)
      (lambda (ef)
        (match ef
          [(nop) '(nop)]
          [(set! ,[(Var env) -> x]
             (,binop ,[(Triv env) -> y] ,[(Triv env) -> z]))
           `(set! ,x (,binop ,y ,z))]
          [(set! ,[(Var env) -> x] ,[(Triv env) -> y])
           (if (eq? y x) `(nop) `(set! ,x ,y))]
					[(mset! ,[(Var env) -> base] ,[(Var env) -> offset] ,[(Var env) -> value]) `(mset! ,base ,offset ,value)]
          [(begin ,[ef] ,[ef*] ...) `(begin ,ef ,ef* ...)]
          [(if ,[(Pred env) -> test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(return-point ,rplab ,[(Tail env) -> tail]) ;;Handling return-point expressions in the Effect 
           	`(return-point ,rplab ,tail)]
          [,ef (error who "invalid Effect ~s" ef)]))))
  (define Tail
    (lambda (env)
      (lambda (tail)
        (match tail
          [(begin ,[(Effect env) -> ef*] ... ,[tail]) `(begin ,ef* ... ,tail)]
          [(if ,[(Pred env) -> test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(,[(Triv env) -> t] ,[(Triv env) -> live*] ...) `(,t ,live* ...)]
          [,tail (error who "invalid Tail ~s" tail)]))))
  (define Body
    (lambda (bd)
      (match bd
        [(locals (,local* ...)
           (ulocals (,ulocal* ...)
             (locate ([,uvar* ,loc*] ...)
               (frame-conflict ,ct ,[(Tail (map cons uvar* loc*)) -> tail]))))
         `(locals (,local* ...)
            (ulocals (,ulocal* ...)
              (locate ([,uvar* ,loc*] ...)
                (frame-conflict ,ct ,tail))))]
        [(locate ([,uvar* ,loc*] ...) ,tail) 
         `(locate ([,uvar* ,loc*] ...) ,tail)]
        [,bd (error who "invalid Body ~s" bd)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> bd*])] ...) ,[Body -> bd])
       `(letrec ([,label* (lambda () ,bd*)] ...) ,bd)]
      [,x (error who "invalid Program ~s" x)])))


;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------

;;; Select instructions requires mrefs and msets to be rewritten substantially in some cases
;;; I prepared a table based on base and offset values
;;;offset		base  ----------->
;;;|					ur	int32	 fvar  label  
;;;|	ur			X1  X1      X3    X3
;;;| int32   X4  X2      X5    X5
;;;| fvar    X6  X2      X5    X5
;;;| label   X6  X2      X5    X5
;;;|
;;;v

;;;X1 => return expression as is
;;;X2 => (set! u base) (set! lhs (mref u offset))
;;;X3 => (set! u offset) (set! lhs (mref base u))
;;;X4 => (set! lhs (mref offset base))
;;;X5 => (set! u1 base) (set! u2 offset) (set! lhs (mref u1 u2))
;;;X6 => first do X4 and then X3

;;; This pretty much handles all cases that there are, mrefs and mset! are handled in the same way

(define-who select-instructions
  (define (ur? x) (or (register? x) (uvar? x)))
  (define (Body x)
    (define new-ulocal* '())
		(define int64-or-label?
		      (lambda (x) (or (and (not (int32? x)) (int64? x)) (label? x))))
    (define (new-u)
      (let ([u (unique-name 't)])
        (set! new-ulocal* (cons u new-ulocal*))
        u))
    (define (select-binop-1 var binop triv1 triv2)
			(cond
         [(eq? var triv1) (select-binop-2 binop var triv2)]
         [(and (eq? var triv2) (member binop '(+ * logor logand))) (select-binop-2 binop var triv1)]
         [else (let ([u (new-u)])
                 `(begin (set! ,u ,triv1) ,(select-binop-2 binop u triv2) (set! ,var ,u)))]))
    (define (select-binop-2 binop var triv) 
			(cond
        [(and (member binop '(- + sra logor logand))
              (or (int64-or-label? triv) (and (frame-var? var) (frame-var? triv))))
         				(let ([u (new-u)])
         						`(begin (set! ,u ,triv) (set! ,var (,binop ,var ,u))))]
        ;;; X2
        [(and (eq? binop '*) (frame-var? var))
         (let ([u (new-u)])
                `(begin (set! ,u ,var) ,(select-binop-2 binop u triv) (set! ,var ,u)))]
        ;;; X1 for *
        [(and (eq? binop '*) (ur? var) (int64-or-label? triv))
         			(let ([u (new-u)])
         					`(begin (set! ,u ,triv) (set! ,var (,binop ,var ,u))))]
        [else `(set! ,var (,binop ,var ,triv))]))
    (define (select-move var triv)
			(if (and (frame-var? var) (or (frame-var? triv) (int64-or-label? triv)))
			            ;;; X0
			            (let ([u (new-u)])
			              `(begin (set! ,u ,triv) (set! ,var ,u)))
			            `(set! ,var ,triv)))
    (define select-relop 
			(lambda (relop x y)
				(cond
					[(and (int32? x) (or (ur? y) (frame-var? y))) `(,(relop^ relop) ,y ,x)]
					[(or (and (frame-var? x) (frame-var? y))
					     (and (int32? x) (int32? y))
					     (and (int64-or-label? x) (or (ur? y) (frame-var? y) (int32? y))))
										(let ([u (new-u)])
										     `(begin (set! ,u ,x) (,relop ,u ,y)))]
					[(and (or (ur? x) (frame-var? x) (int32? x))
					      (int64-or-label? y))
					           (let ([u (new-u)])
					                  `(begin (set! ,u ,y) (,(relop^ relop) ,u ,x)))]
					[(and (int64-or-label? x) (int64-or-label? y))
					           (let ([u1 (new-u)] [u2 (new-u)])
					                  `(begin (set! ,u1 ,x) (set! ,u2 ,y) (,relop ,u1 ,u2)))]
					[else `(,relop ,x ,y)])))
		;;;select mref and mset do the same thing the only difference being that the former returns mref expressions and the latter returns mset!
		(define select-mref
			(lambda (lhs base offset)
				(cond
					[(or (and (ur? base) (integer? offset))
					 		 (and (ur? base) (ur? offset))) `((set! ,lhs (mref ,base ,offset)))]	;;X1
					[(and (integer? offset) (or (integer? base) (frame-var? base) (label? base)))
							(let ((u (new-u)))
								`((set! ,u ,base) (set! ,lhs (mref ,u ,offset))))]									;;X2
					[(and (ur? base) (or (frame-var? offset) (label? offset)))
								(let ((u (new-u)))
															`((set! ,u ,offset) (set! ,lhs (mref ,base ,u))))]
					[(and (ur? offset) (or (frame-var? base) (frame-var? offset)))
												(select-mref lhs offset base)]
					[else (let ((u1 (new-u)) (u2 (new-u)))
											`((set! ,u1 ,base) (set! ,u2 ,offset) (set! ,lhs (mref ,u1 ,u2))))])))
		(define select-mset
			(lambda (base offset value)
				(cond
					[(or (and (ur? base) (integer? offset))
					 		 (and (ur? base) (ur? offset))) `((mset! ,base ,offset ,value))]
					[(and (integer? offset) (or (integer? base) (frame-var? base) (label? base)))
							(let ((u (new-u)))
								`((set! ,u ,base) (mset! ,u ,offset ,value) (set! ,base ,u)))]
					[(and (ur? base) (or (frame-var? offset) (label? offset)))
								(let ((u (new-u)))
												`((set! ,u ,offset) (mset! ,base ,u ,value)))]
					[(and (ur? offset) (or (frame-var? base) (frame-var? offset)))
												(select-mset offset base value)]
					[else (let ((u1 (new-u)) (u2 (new-u)))
									`((set! ,u1 ,base) (set! ,u2 ,offset) (mset! ,u1 ,u2 ,value)))])))
    (define Effect 
			(lambda (ef)
				(match ef
	        [(nop) '(nop)]
	        [(begin ,[Effect -> ef*] ... ,[Effect -> ef]) (make-begin `(,ef* ... ,ef))]
	        [(if ,[Pred -> test] ,[Effect -> conseq] ,[Effect -> altern]) `(if ,test ,conseq ,altern)]
					[(set! ,lhs (mref ,base ,offset))
						(cond
							[(and (integer? base) (ur? offset)) (make-begin (select-mref lhs offset base))] ;;exchange base and offset
							[(ur? lhs) (make-begin (select-mref lhs base offset))] ;;;pass lhs as it is
							[(frame-var? lhs)
								(let ((u (new-u)))
									 (make-begin `((set! ,u ,lhs) ,(select-mref u base offset) ... (set! ,lhs ,u))))] ;;;if frame-var assign a new unspillable and assign it back
							[(label? lhs) 					
								(let ((u (new-u)))
									 (make-begin `((set! ,u ,lhs) ,(select-mref u base offset) ...)))] ;;;if lhs a label assign new unspillable and call select-mref passing new lhs
							[else '(nop)])]
	        [(set! ,lhs (,binop ,x ,y)) (select-binop-1 lhs binop x y)]
	        [(set! ,lhs ,rhs) (select-move lhs rhs)]
					[(mset! ,base ,offset ,value)
						(cond 
							[(and (integer? base) (ur? offset)) (make-begin (select-mset offset base value))]
							[(or (ur? value) (integer? value)) (make-begin (select-mset base offset value))]
							[(frame-var? value)
								(let ((u (new-u)))
									(make-begin `((set! ,u ,value) ,(select-mset base offset u) ... (set! ,value ,u))))]
							[(label? value)
							 	(let ((u (new-u)))
									(make-begin `((set! ,u ,value) ,(select-mset base offset u) ...)))])]
	        [(return-point ,rplab ,[Tail -> tail]) `(return-point ,rplab ,tail)] ;;;Process tail and send the rest of instructions as is
	        [,x (error who "invalid Effect ~s" x)])))
    (define (Pred x) 
			(match x
				[(true) '(true)]
				[(false) '(false)]
				[(if ,[Pred -> pred] ,[Pred -> conseq] ,[Pred -> alt])
					`(if ,pred ,conseq ,alt)]
				[(begin ,[Effect -> ef*] ...,[Pred -> tail]) 
					(make-begin `(,ef* ... ,tail))]
				[(,relop ,conseq ,alt) (select-relop relop conseq alt)]))
    (define (Tail x)
			(match x
				[(begin ,[Effect -> ef*] ... ,[Tail -> tail])
					(make-begin `(,ef* ... ,tail))]
				[(if ,[Pred -> pred] ,[Tail -> conseq] ,[Tail -> alt]) 
					`(if ,pred ,conseq ,alt)]
				[(,loc* ...) `(,loc* ...)]))
    (match x
      [(locals (,local* ...) 
         (ulocals (,ulocal* ...)
           (locate (,home* ...) (frame-conflict ,ct ,[Tail -> tail]))))
       `(locals (,local* ...)
          (ulocals (,ulocal* ... ,new-ulocal* ...)
            (locate (,home* ...)
              (frame-conflict ,ct ,tail))))]
      [(locate (,home* ...) ,tail) `(locate (,home* ...) ,tail)]
      [,x (error who "invalid Body ~s" x)]))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,x (error who "invalid Program ~s" x)])))


;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------

;;;uncover register conflicts needs to handle mrefs and mset's in different way while updating the conflict table
;;;(set! x (mref base offset)) => will mean that x conflicts with both base and offset and hence we must record these conflicts
;;;(mset! base offset value) => in this we have to record the conflicts of each parameters with the other 

(define-who uncover-register-conflict
  (define add-conflicts!
	    (lambda (ct lhs live*)
	      (define add-conflict!
	        (lambda (var1 var2)
	          (let ([a (assq var1 ct)])
	            (set-cdr! a (if (eq? var1 var2) (cdr a)  (set-cons var2 (cdr a)))))))
	      (when (uvar? lhs)
	        (for-each
	          (lambda (live) (add-conflict! lhs live))
	          live*))
	      (for-each
	        (lambda (live) (when (and (uvar? live) (not (frame-var? lhs))) (add-conflict! live lhs)))
	        live*)))
  (define Triv (lambda (x) (if (or (uvar? x) (register? x)) `(,x) '())))
  (define Effect*
    (lambda (x live* ct)
      (match x
        [() live*]
        [(,ef* ... ,ef) (Effect* ef* (Effect ef live* ct) ct)]
        [,x (error who "invalid Effect* list ~s" x)])))
  (define Effect
    (lambda (x live* ct)
      (match x
        [(nop) live*]
        [(if ,test ,[c-live*] ,[a-live*]) 
				(Pred test  c-live* a-live* ct)]
				[(mset! ,[Triv -> base] ,[Triv -> offset] ,[Triv -> value])
					(begin
						(if (not (null? base)) (add-conflicts! ct (car base) (union offset value live*)))
						(if (not (null? offset)) (add-conflicts! ct (car offset) (union base value live*)))
						(if (not (null? value)) (add-conflicts! ct (car value) (union base offset live*)))
						(union base offset value live*))]
        [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
				[(set! ,lhs (mref ,[Triv -> x-live*] ,[Triv -> y-live*]))
							          (begin
											(add-conflicts! ct lhs (union x-live* y-live* live*))
											(union x-live* y-live* (remq lhs live*)))]
        [(set! ,lhs (,binop ,[Triv -> x-live*] ,[Triv -> y-live*]))
         	(begin
							(add-conflicts! ct lhs live*)
							(union x-live* y-live* (remq lhs live*)))]
        [(set! ,lhs ,var)
         	(begin
						(add-conflicts! ct lhs live*)
						(if (or (uvar? var) (register? var)) (set-cons var (remq lhs live*)) (remq lhs live*)))]
       ; ignoring incoming live*, since it should not contain anything
       ; but caller-save registers, which the call kills (see note in
       ; the assignment description)
        [(return-point ,rplab ,tail) (Tail tail ct)] ;;;Return the list of variables live in the tail ignoring the variables that were live before the call was made
        [,x (error who "invalid Effect list ~s" x)])))
  (define Pred
    (lambda (x t-live* f-live* ct)
      (match x
        [(true) t-live* ]
        [(false) f-live* ]
        [(if ,test ,[c-live*] ,[a-live*]) (union t-live* f-live* (Pred test c-live* a-live* ct))]
        [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
        [(,predop ,[Triv -> x-live*] ,[Triv -> y-live*])
         (union x-live* y-live* t-live* f-live*)]
        [,x (error who "invalid Pred ~s" x)])))
  (define Tail
    (lambda (x ct)
      (match x
        [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
        [(if ,test ,[c-live*] ,[a-live*]) (Pred test c-live* a-live* ct)]
        [(,[Triv -> target-live*] ,[Triv -> live*] ...) `(,target-live* ... ,live* ... ...)]
        [,x (error who "invalid Tail ~s" x)])))
  (define Body
    (lambda (x)
      (match x
        [(locals (,local* ...) 
           (ulocals (,ulocal* ...)
             (locate (,home* ...)
               (frame-conflict ,fv-ct ,tail))))
         (let ([ct (map (lambda (x) (cons x '())) `(,local* ... ,ulocal* ...))])
           (let ([uvar* (filter uvar? (Tail tail ct))])
             (unless (null? uvar*)
               (error who "found variables ~s live on entry" uvar*)))
           `(locals (,local* ...) 
              (ulocals (,ulocal* ...)
                (locate (,home* ...)
                  (frame-conflict ,fv-ct
                    (register-conflict ,ct ,tail))))))]
        [(locate (,home* ...) ,tail) `(locate (,home* ...) ,tail)]
        [,x (error who "invalid Body ~s" x)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,x (error who "invalid Program ~s" x)])))


;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------

;;assigns register to the uvars made a chnage documented below

(define-who assign-registers
  (define remove-occurence						;;Removes the occurence of a var from var* and returns the list
			(lambda (var ct)
				(map (lambda (x) 
								(cond
									[(eq? (car x) var) x]
									[else (remq var x)])) ct)))
		(define replace																		;;Replaces the occurences of variables in the conflict-list with the register-homes
			(lambda (allocations ct)
				(cond 
					[(null? allocations) ct]
					[else (replace (cdr allocations) (replace-helper (car allocations) ct))])))
		(define replace-helper
			(lambda (allocation ct)
				(map (lambda (ct-entry)
								(cond
									[(eq? (car allocation) (car ct-entry)) allocation]
									[else (cons (car ct-entry) (replace* (cdr ct-entry) allocation))])) ct)))
		(define replace*
			(lambda (conflicts allocation)
				(cond
					[(null? conflicts) '()]
					[(eq? (car conflicts) (car allocation)) (cons (cadr allocation) (replace* (cdr conflicts) allocation))]
					[else (cons (car conflicts) (replace* (cdr conflicts) allocation))])))			
		(define k (length registers))
	  (define low-degree?
	    (lambda (var ct)
	      (< (length (cdr (assq var ct))) k)))
	(define num-conflicts
		(lambda (var ct)
			(let ((temp (assq var ct)))
				(if (null? temp) 2000 (length (cdr (assq var ct)))))))
	(define pick-min																										;;Picks a node with least number of conflicts like the min function
		(lambda (var degree var* ct)
			(cond
				[(null? var) 'xxx]
				[(null? var*) var]
				[(<= degree (num-conflicts (car var*) ct)) (pick-min var degree (cdr var*) ct)]
				[else (let* ((node (car var*))
										(degree^ (num-conflicts node ct)))
										(pick-min node degree^ (cdr var*) ct))])))
	(define assign-null
		(lambda (ls)
			(if (null? ls) 'xxx (car ls))))
	(define uncover-register-bias
		(lambda (body bias-list)
			(match body
				[(begin ,ef* ...) (Effect* ef* bias-list)])))
	(define Effect*
		(lambda (effect* bias-list)
			(match effect*
				[,x (guard (null? x)) bias-list]
				[(,ef,ef* ...) (Effect* ef* (Effect ef bias-list))])))
	(define Effect
		(lambda (ef bias-list)
			(match ef
				[(set! ,x ,y) (guard (and (uvar? x) (uvar? y)))
				 							(begin
												(set-cdr! (assq x bias-list) (set-cons y (cdr (assq x bias-list))))
												(set-cdr! (assq y bias-list) (set-cons x (cdr (assq y bias-list))))
												bias-list)]
				[(set! ,x ,y) (guard (and (uvar? x) (register? y)))
											  (begin
													(set-cdr! (assq x bias-list) (set-cons y (cdr (assq x bias-list))))
													bias-list)]
				[(set! ,x ,y) (guard (and (register? x) (uvar? y)))
											  (begin
													(set-cdr! (assq y bias-list) (set-cons x (cdr (assq y bias-list))))
													bias-list)]
				[,x bias-list])))
	;;;altered the function here as advised by Andy Keep 
	;;;first I will assign all the variables in spillable list and then on the unspillable list
	(define find-homes
	    (lambda (spillable* unspillable* ct)
	      (cond
					[(and (null? spillable*) (null? unspillable*)) '()]
					[(null? spillable*) (find-homes unspillable* '() ct)]
					[else (let* ((current-var (pick-min (car spillable*) (num-conflicts (car spillable*) ct) (cdr spillable*) ct))
											(new-conflict-table (remove-occurence current-var ct))
											(results (find-homes (remq current-var spillable*) (remq current-var unspillable*) new-conflict-table))
											(updated-ct (replace results ct))
											(conflict-entry (cdr (assq current-var updated-ct)))
									 	  (remaining-registers (difference registers conflict-entry)))
										 (if (null? remaining-registers) 
												results 
												(let ((assign-register (car remaining-registers)))
													(cons (list current-var assign-register) results))))])))
		(define get-replacement
			(lambda (var entry)
						(list var (car (difference registers entry)))))
		(define get-register
			(lambda (ls conflict-entry)
				(cond
					[(null? ls) #f]
					[(and (register? (car ls)) (not (memq (car ls) conflict-entry))) (car ls)]
					[else (get-register (cdr ls) conflict-entry)])))
		(define update-bias-table
			(lambda (uvar reg bt)
				(map (lambda (entry) 
								(let ([tail (cdr entry)])
									(if (memq uvar tail)
											(begin
												(set-cdr! entry (cons reg (remq uvar entry)))
												entry)
												entry))) bt)))
		(define assign-homes
			(lambda (var* bt ct)
				(cond
					[(null? bt) '() ]
					[(null? var*) '()]
					[else (let* ([current-var (car var*)]
											 [bt-entry (assq current-var bt)]
											 [friend-list (cdr bt-entry)]
											 [conflict-entry (cdr (assq current-var ct))])
													(if (null? friend-list) 
															(assign-homes (cdr var*) bt ct)
															(let ([friend-register (get-register friend-list conflict-entry)])
																(cond
																	[(eq? friend-register #f) (assign-homes (cdr var*) bt ct)]
																	[else (let ([updated-ct (replace (list (list current-var friend-register)) ct)]
																							[updated-bt (update-bias-table current-var friend-register bt)])
																							(cons (list current-var friend-register) (assign-homes (cdr var*) updated-bt updated-ct)))]))))])))
  (define Body
    (lambda (x)
      (match x
        [(locals (,local* ...) 
           (ulocals (,ulocal* ...)
             (locate (,frame-home* ...)
               (frame-conflict ,fv-ct
                 (register-conflict ,ct ,tail)))))
         ;; putting local* before ulocal* allows find-homes to choose the
         ;; first element of the list when all variables are high degree and
         ;; be guaranteed a spillable variable if one is left.  if find-homes
         ;; wants to be more clever about choosing a high-degree victim, it
         ;; will have to be told which variables are spillable.
         (let* ([uvar* (append local* ulocal*)]
							  [bias-list (move-bias tail register? uvar*)])
           (let* ( [biased-home* (assign-homes uvar* bias-list ct)]
									 [homes (if (null? biased-home*) '() (map car biased-home*))]
									 [new-ct (if (null? biased-home*) ct (replace biased-home* ct))]
									 [home* (find-homes (difference local* homes) (difference ulocal* homes) new-ct)])
             (let ([spill* (difference uvar* (union (map car home*) homes))])
               (cond
                 [(null? spill*) `(locate (,frame-home* ... ,biased-home* ... ,home* ...) ,tail)]
                 [(null? (intersection ulocal* spill*))
                  (let ([local* (difference local* spill*)])
                    `(locals (,local* ...)
                       (ulocals (,ulocal* ...)
                         (spills (,spill* ...)
                           (locate (,frame-home* ...)
                             (frame-conflict ,fv-ct ,tail))))))]
                 [else 
                  (error who "unspillable variables (~s) have been spilled"
                    (difference spill* local*))]))))]
        [(locate (,home* ...) ,tail) `(locate (,home* ...) ,tail)]
        [,x (error who "invalid Body ~s" x)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,x (error who "invalid Program ~s" x)])))


;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------

(define-who everybody-home?
  (define all-home?
    (lambda (body)
      (match body
        [(locals (,local* ...)
           (ulocals (,ulocal* ...)
             (spills (,spill* ...)
               (locate (,home* ...)
                 (frame-conflict ,ct ,tail))))) #f]
        [(locate (,home* ...) ,tail) #t]
        [,x (error who "invalid Body ~s" x)])))
  (lambda (x)
    (match x
       [(letrec ([,label* (lambda () ,body*)] ...) ,body)
        (andmap all-home? `(,body ,body* ...))]
       [,x (error who "invalid Program ~s" x)])))


;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------

;;;pass changed since last assignment

(define-who assign-frame
		(define replace																		;;Replaces the occurences of variables in the conflict-list with the register-homes
			(lambda (allocations ct)
				(cond 
					[(null? allocations) ct]
					[else (replace (cdr allocations) (replace-helper (car allocations) ct))])))
		(define replace-helper
			(lambda (allocation ct)
				(map (lambda (ct-entry)
								(cond
									[(eq? (car allocation) (car ct-entry)) allocation]
									[else (cons (car ct-entry) (replace* (cdr ct-entry) allocation))])) ct)))
		(define replace*
			(lambda (conflicts allocation)
				(cond
					[(null? conflicts) '()]
					[(eq? (car conflicts) (car allocation)) (cons (cadr allocation) (replace* (cdr conflicts) allocation))]
					[else (cons (car conflicts) (replace* (cdr conflicts) allocation))])))			
		(define update-bias-table
			(lambda (uvar reg bt)
				(map (lambda (entry) 
								(let ([tail (cdr entry)])
									(if (memq uvar tail)
											(begin
												(set-cdr! entry (cons reg (remq uvar entry)))
												entry)
												entry))) bt)))
		(define update-conflict-table
			(lambda (uvar reg ct)
				(map (lambda (entry) 
								(let ([tail (cdr entry)])
									(if (memq uvar tail)
											(begin
												(set-cdr! entry (cons reg (remq uvar entry)))
												entry)
												entry))) ct)))
		(define get-frame
			(lambda (ls conflict-entry)
				(cond
					[(null? ls) #f]
					[(and (frame-var? (car ls)) (not (memq (car ls) conflict-entry))) (car ls)]
					[else (get-frame (cdr ls) conflict-entry)])))
	(define assign-homes
		(lambda (var* bt ct)
			(cond
				[(null? bt) '() ]
				[(null? var*) '()]
				[else (let* ([current-var (car var*)]
										 [bt-entry (assq current-var bt)]
										 [friend-list (cdr bt-entry)]
										 [conflict-entry (cdr (assq current-var ct))])
												(if (null? friend-list) 
														(assign-homes (cdr var*) bt ct)
														(let ([friend-frame (get-frame friend-list conflict-entry)])
															(cond
																[(eq? friend-frame #f) (assign-homes (cdr var*) bt ct)]
																[else (let ([updated-ct (update-conflict-table current-var friend-frame ct)]
																						[updated-bt (update-bias-table current-var friend-frame bt)])
																						(cons (list current-var friend-frame) (assign-homes (cdr var*) updated-bt updated-ct)))]))))])))
  (define find-used
    (lambda (conflict* home*)
      (cond
        [(null? conflict*) '()]
        [(frame-var? (car conflict*)) 
         (set-cons (car conflict*) (find-used (cdr conflict*) home*))]
        [(assq (car conflict*) home*) => 
         (lambda (x) (set-cons (cadr x) (find-used (cdr conflict*) home*)))]
        [else (find-used (cdr conflict*) home*)])))
  (define find-frame-var
    (lambda (used*)
      (let f ([index 0])
        (let ([fv (index->frame-var index)])
          (if (memq fv used*) (f (+ index 1)) fv)))))
  (define find-homes
    (lambda (var* ct home*)
      (if (null? var*)
          home*
          (let ([var (car var*)] [var* (cdr var*)])
            (let ([conflict* (cdr (assq var ct))])
              (let ([home (find-frame-var (find-used conflict* home*))])
                (find-homes var* ct `((,var ,home) . ,home*))))))))
  (define Body
    (lambda (body)
      (match body
        [(locals (,local* ...)
           (ulocals (,ulocal* ...)
             (spills (,spill* ...)
               (locate (,home* ...)
                 (frame-conflict ,ct ,tail)))))
         (let* (				[uvar* (union ulocal* local* spill*)]
																[bias-list (move-bias tail frame-var? uvar*)]
																[biased-home* (assign-homes spill* bias-list ct)]
																[home (if (null? biased-home*) '() (map car biased-home*))]
															  [new-ct (if (null? biased-home*) ct (replace biased-home* ct))]
															  [home* (find-homes (difference spill* home) ct home*)])
           `(locals (,local* ...)
              (ulocals (,ulocal* ...)
                (locate (,biased-home* ... ,home* ...)
                  (frame-conflict ,ct ,tail)))))]
        [(locate (,home* ...) ,body) `(locate (,home* ...) ,body)]
        [,body (error who "invalid Body ~s" body)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,x (error who "invalid Program ~s" x)])))


;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------

;;; mref and mset! are allowed to go as they are, as they are not affected by what this pass does

(define-who discard-call-live
  (define Tail
    (lambda (tail)
      (match tail
        [(begin ,[Effect -> ef*] ... ,[Tail -> tail]) `(begin ,ef* ... ,tail)]
        [(if ,[Pred -> test] ,[Tail -> conseq] ,[Tail -> altern]) `(if ,test ,conseq ,altern)]
        [(,t ,live* ...) `(,t)]
        [,tail (error who "invalid Tail ~s" tail)])))
  (define Pred
    (lambda (pr)
      (match pr
        [(true) '(true)]
        [(false) '(false)]
        [(if ,[Pred -> test] ,[Pred -> conseq] ,[Pred -> altern]) `(if ,test ,conseq ,altern)]
        [(begin ,[Effect -> ef*] ... ,[Pred -> pr]) `(begin ,ef* ... ,pr)]
        [(,predop ,x ,y) `(,predop ,x ,y)]
        [,pr (error who "invalid Pred ~s" pr)])))
  (define Effect
    (lambda (ef)
      (match ef
        [(nop) '(nop)]
        [(set! ,x ,rhs) `(set! ,x ,rhs)]
				[(mset! ,base ,offset ,value) ef]
        [(begin ,[Effect -> ef*] ... ,[Effect -> ef]) `(begin ,ef* ... ,ef)]
        [(if ,[Pred -> test] ,[Effect -> conseq] ,[Effect -> altern]) `(if ,test ,conseq ,altern)]
        [(return-point ,rplab ,[Tail -> tail]) `(return-point ,rplab ,tail)]
        [,ef (error who "invalid Effect ~s" ef)])))
  (define Body
    (lambda (bd)
      (match bd
        [(locate ([,uvar* ,loc*] ...) ,[Tail -> tail])
         `(locate ([,uvar* ,loc*] ...) ,tail)]
        [,bd (error who "invalid Body ~s" bd)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> bd*])] ...) ,[Body -> bd])
       `(letrec ([,label* (lambda () ,bd*)] ...) ,bd)]
      [,x (error who "invalid Program ~s" x)])))


;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------

;;;finalize locations adds clauses for this weeks grammars and handles the mset! and mref clauses...If any of the arguments are assigned a frame-var
;;; or register it is assigned here

(define-who finalize-locations
  (define Var
    (lambda (env)
      (lambda (v)
        (if (uvar? v) (cdr (assq v env)) v))))
  (define Triv
    (lambda (env)
      (lambda (t)
        (if (uvar? t) (cdr (assq t env)) t))))
  (define Pred
    (lambda (env)
      (lambda (pr)
        (match pr
          [(true) '(true)]
          [(false) '(false)]
          [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
          [(begin ,[(Effect env) -> ef*] ... ,[pr]) `(begin ,ef* ... ,pr)]
          [(,predop ,[(Triv env) -> x] ,[(Triv env) -> y]) `(,predop ,x ,y)]
          [,pr (error who "invalid Pred ~s" pr)]))))
  (define Effect
    (lambda (env)
      (lambda (ef)
        (match ef
          [(nop) '(nop)]
          [(set! ,[(Var env) -> x]
             (,binop ,[(Triv env) -> y] ,[(Triv env) -> z]))
           `(set! ,x (,binop ,y ,z))]
          [(set! ,[(Var env) -> x] ,[(Triv env) -> y]) 
           (if (eq? y x) `(nop) `(set! ,x ,y))]
					[(mset! ,[(Var env) -> base] ,[(Var env) -> offset] ,[(Var env) -> val])
						`(mset! ,base ,offset ,val)]
          [(begin ,[ef] ,[ef*] ...) `(begin ,ef ,ef* ...)]
          [(if ,[(Pred env) -> test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(return-point ,rplab ,[(Tail env) -> tail])
           	`(return-point ,rplab ,tail)]
          [,ef (error who "invalid Effect ~s" ef)]))))
  (define Tail
    (lambda (env)
      (lambda (tail)
        (match tail
          [(begin ,[(Effect env) -> ef*] ... ,[tail]) `(begin ,ef* ... ,tail)]
          [(if ,[(Pred env) -> test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(,[(Triv env) -> t]) `(,t)]
          [,tail (error who "invalid Tail ~s" tail)]))))
  (define Body
    (lambda (bd)
      (match bd
        [(locate ([,uvar* ,loc*] ...) ,tail) ((Tail (map cons uvar* loc*)) tail)]
        [,bd (error who "invalid Body ~s" bd)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> bd*])] ...) ,[Body -> bd])
       `(letrec ([,label* (lambda () ,bd*)] ...) ,bd)]
      [,x (error who "invalid Program ~s" x)])))


;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------

;;The triv function checks to see if a frame var has been encountered and accordingly makes the displacement operand
;; To each function we pass the offset on return from a tail expression offset must be set to zero
;;; Finally we get rid of the mref and mset! expressions here
;;; (mset! x y z) means x[y] = z 
;;; if x is assigned a register and y a int then I make a displacement operand
;;; if x and y are assigned registers then I make an index operands
;;; otherwise I simply swap x and y and make a displacement operand ditto while handling the mrefs 
;;; otherwise the pass remains identical to last weeks pass

(define-who expose-frame-var
  (define Triv
    (lambda (fp-offset)
      (lambda (t)
        (if (frame-var? t)
            (make-disp-opnd frame-pointer-register
              (- (ash (frame-var->index t) align-shift) fp-offset))
            t))))
  (define Pred
    (lambda (pr fp-offset)
      (match pr
        [(true) (values '(true) fp-offset)]
        [(false) (values '(false) fp-offset)]
        [(begin ,ef* ... ,pr)
         (let-values ([(ef* fp-offset) (Effect* ef* fp-offset)])
           (let-values ([(pr fp-offset) (Pred pr fp-offset)])
             (values (make-begin `(,ef* ... ,pr)) fp-offset)))]
        [(if ,test ,conseq ,altern)
         (let-values ([(test fp-offset) (Pred test fp-offset)])
           (let-values ([(conseq c-fp-offset) (Pred conseq fp-offset)]
                        [(altern a-fp-offset) (Pred altern fp-offset)])
             (values `(if ,test ,conseq ,altern) c-fp-offset)))]
        [(,predop ,[(Triv fp-offset) -> tr1] ,[(Triv fp-offset) -> tr2])
         (values `(,predop ,tr1 ,tr2) fp-offset)]
        [,pr (error who "invalid Pred ~s" pr)])))
  (define Effect*
    (lambda (ef* fp-offset)
      (if (null? ef*)
          (values '() fp-offset)
          (let-values ([(ef fp-offset) (Effect (car ef*) fp-offset)])
            (let-values ([(ef* fp-offset) (Effect* (cdr ef*) fp-offset)])
              (values (cons ef ef*) fp-offset))))))
  (define Effect
    (lambda (st fp-offset)
      (match st
        [(nop) (values '(nop) fp-offset)]
				[(mset! ,[(Triv fp-offset) -> base] ,[(Triv fp-offset) -> offset] ,[(Triv fp-offset) -> value])
					(cond
						[(and (int32? base) (register? offset)) (values `(set! ,(make-disp-opnd offset base) ,value) fp-offset)]
						[(and (int32? offset) (register? base)) (values `(set! ,(make-disp-opnd base offset) ,value) fp-offset)]
						[else (values `(set! ,(make-index-opnd base offset) ,value) fp-offset)])]
        [(set! ,fp (+ ,fp ,n))
         (guard (eq? fp frame-pointer-register))
         	(values st (+ fp-offset n))]  ;;send the new offset as incoming offset + n
        [(set! ,fp (- ,fp ,n))
         (guard (eq? fp frame-pointer-register))
         	(values st (- fp-offset n))] 	;;send the new offset as incoming offset - n
				[	(set! ,[(Triv fp-offset) -> var]
	           (mref ,[(Triv fp-offset) -> t1] ,[(Triv fp-offset) -> t2]))
						(cond
							[(and (register? t1) (register? t2)) (values `(set! ,var ,(make-index-opnd t1 t2)) fp-offset)]
							[(and (register? t1) (int32? t2)) (values `(set! ,var ,(make-disp-opnd t1 t2)) fp-offset)]
							[(and (int32? t1) (register? t2)) (values `(set! ,var ,(make-disp-opnd t2 t1)) fp-offset)])]
        [(set! ,[(Triv fp-offset) -> var]
           (,binop ,[(Triv fp-offset) -> t1] ,[(Triv fp-offset) -> t2]))
         (values `(set! ,var (,binop ,t1 ,t2)) fp-offset)]
        [(set! ,[(Triv fp-offset) -> var] ,[(Triv fp-offset) -> t])
         (values `(set! ,var ,t) fp-offset)]
        [(begin ,ef* ... ,ef)
         		(let-values ([(ef* fp-offset) (Effect* ef* fp-offset)])
		           (let-values ([(ef fp-offset) (Effect ef fp-offset)])
		             (values (make-begin `(,ef* ... ,ef)) fp-offset)))]
        [(if ,test ,conseq ,altern)
         		(let-values ([(test fp-offset) (Pred test fp-offset)])
		           (let-values ([(conseq c-fp-offset) (Effect conseq fp-offset)]
		                        [(altern a-fp-offset) (Effect altern fp-offset)])
		             (values `(if ,test ,conseq ,altern) c-fp-offset)))]
        [(return-point ,rplab ,[(Tail fp-offset) -> tail fp-offset])			;; Process the tail expression, get the same offset and return it
         	(values `(return-point ,rplab ,tail) fp-offset)]
        [,st (error who "invalid syntax for Effect ~s" st)])))
  (define Tail
    (lambda (fp-offset)
      (lambda (tail)
        (match tail
          [(begin ,ef* ... ,tail)
           (let-values ([(ef* fp-offset) (Effect* ef* fp-offset)])
	           (let-values ([(tl fp-offset) ((Tail fp-offset) tail)])
	             (values (make-begin `(,ef* ... ,tl)) fp-offset)))]
          [(if ,test ,conseq ,altern)
          	(let-values ([(test fp-offset) (Pred test fp-offset)])
		           (let-values ([(conseq c-fp-offset) ((Tail fp-offset) conseq)]
		                        [(altern a-fp-offset) ((Tail fp-offset) altern)])
		             (values `(if ,test ,conseq ,altern) c-fp-offset)))]
					[(,[(Triv fp-offset) -> t]) (values `(,t) fp-offset)]
          [,tail (error who "invalid syntax for Tail ~s" tail)]))))
  (define Body
    (lambda (x)
      (let-values ([(x fp-offset) ((Tail 0) x)])
        (unless (= fp-offset 0)
          (error who "nonzero final fp-offset ~s" fp-offset))
        x)))
  (lambda (program)
    (match program
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,program (error who "invalid syntax for Program: ~s" program)])))


;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------

;;; Doesnot change from previous assignment

(define-who expose-basic-blocks
  (define Tail
		(lambda (x)
	    (match x
	      [(if ,pred ,[conseq cb*] ,[altern ab*])
	       (let ([clab (unique-label 'c)] [alab (unique-label 'a)])
	         (let-values ([(tail xb*) (Pred pred clab alab)])
	           (values tail
	             `(,xb* ...
	               [,clab (lambda () ,conseq)]
	               [,alab (lambda () ,altern)]
	               ,cb* ...
	               ,ab* ...))))]
	      [(begin ,effect* ... ,[tail tb*])
	       (let-values ([(expr eb*) (Effect* effect* `(,tail))])
	         (values expr `(,eb* ... ,tb* ...)))]
	      [(,triv) (values `(,triv) '())]
	      [,x (error who "invalid Tail ~s" x)])))
  (define (Pred x tlab flab)
    (match x
      [(true) (values `(,tlab) '())]
      [(false) (values `(,flab) '())]
      [(if ,pred ,[conseq cb*] ,[altern ab*])
       (let ([clab (unique-label 'c)] [alab (unique-label 'a)])
         (let-values ([(expr xb*) (Pred pred clab alab)])
           (values expr
             `(,xb* ...
               [,clab (lambda () ,conseq)]
               [,alab (lambda () ,altern)]
               ,cb* ...
               ,ab* ...))))]
      [(begin ,effect* ... ,[expr xb*])
       (let-values ([(expr eb*) (Effect* effect* `(,expr))])
         (values expr `(,eb* ... ,xb* ...)))]
      [(,relop ,triv1 ,triv2)
       (values `(if (,relop ,triv1 ,triv2) (,tlab) (,flab)) '())]
      [,x (error who "invalid Tail ~s" x)]))
  (define (Effect* x* rest*)
    (match x*
      [() (values (make-begin rest*) '())]
      [(,x* ... ,x) (Effect x* x rest*)]))
  (define Effect 
		(lambda (x* x rest*)
	    (match x
	      [(nop) (Effect* x* rest*)]
	      [(set! ,lhs ,rhs) (Effect* x* `((set! ,lhs ,rhs) ,rest* ...))]
	      [(if ,pred ,conseq ,altern)
	       (let ([clab (unique-label 'c)]
	             [alab (unique-label 'a)]
	             [jlab (unique-label 'j)])
	         (let-values ([(conseq cb*) (Effect '() conseq `((,jlab)))]
	                      [(altern ab*) (Effect '() altern `((,jlab)))]
	                      [(expr xb*) (Pred pred clab alab)])
	           (let-values ([(expr eb*) (Effect* x* `(,expr))])
	             (values expr
	               `(,eb* ...
	                 ,xb* ...
	                 [,clab (lambda () ,conseq)]
	                 [,alab (lambda () ,altern)]
	                 [,jlab (lambda () ,(make-begin rest*))]
	                 ,cb* ...
	                 ,ab* ...)))))]
	      [(begin ,effect* ...) (Effect* `(,x* ... ,effect* ...) rest*)]
	      [(return-point ,rplab ,tail)
	       	(let*-values ([(tail tail-label*) (Tail tail)] [(ef* ef-label*) (Effect* x* (cdr tail))])
						(values (make-begin `(,ef*))
										`(,ef-label* ... ,tail-label* ... [,rplab (lambda () ,(make-begin rest*))])))]
	      [,x (error who "invalid Effect ~s" x)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Tail -> tail* b**])] ...) ,[Tail -> tail b*])
 				`(letrec ([,label* (lambda () ,tail*)] ... ,b** ... ... ,b* ...) ,tail)]
      [,x (error who "invalid Program ~s" x)])))

;(optimize-jumps `(letrec ([,label* (lambda () ,tail*)] ... ,b** ... ... ,b* ...) ,tail))

;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------

;;; Doesnot change from previous assignment

(define flatten-program
	(lambda (prog)
	(define build-exp
		(lambda (label* tail*)
			(match label*
				[() '()]
				[(,current ,rest* ...)
					(let ((current-exp (append (list current) (Tail rest* (car tail*)))))
						(append current-exp (build-exp rest* (cdr tail*))))])))
	(define Prog
		(lambda (x)
			(match x
				[(letrec ([,label* (lambda () ,tail*)] ...) ,tail)
					(let ((tail-exp (Tail label* tail)) (rest-of-exp (build-exp label* tail*)))
						(append '(code) tail-exp rest-of-exp))])))
	(define Tail
		(lambda (label* x)
			(match x
				[(if ,pred (,conseq) (,alt)) 
					(if (null? label*) `((if ,pred (jump ,conseq)) (jump ,alt))
						(let ((next-label (car label*)))
							(cond
								[(eq? next-label conseq) `((if (not ,pred) (jump ,alt)))]
								[(eq? next-label alt) `((if ,pred (jump ,conseq)))]
								[else `((if ,pred (jump ,conseq)) (jump ,alt))])))]
				[(begin ,effect* ...,tail) (append effect* (Tail label* tail))]		
				[(,triv) `((jump ,triv))])))
				(Prog prog)))


;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------

;;; Doesnot change from previous assignment

(define-who generate-x86-64
  (lambda (x)
    (define Program
      (lambda (x)
        (match x
          [(code ,st* ...)
           (emit-program (for-each Stmt st*))])))
    (define Stmt
      (lambda (x)
        (match x
          [(jump ,target) (emit-jump 'jmp target)]
          [(if (,op ,x ,y) (jump ,lbl)) (begin (emit 'cmpq y x) (emit-jump (op->inst op) lbl))]
          [(if (not (,op ,x ,y)) (jump ,lbl)) (begin (emit 'cmpq y x) (emit-jump (inst->inst^ (op->inst op)) lbl))]
          [(set! ,v1 (,op ,v1 ,v2)) (emit (op->inst op) v2 v1)]
          [(set! ,v1 ,v2) (guard (label? v2)) (emit 'leaq v2 v1)]
					[(set! ,v1 ,v2) (emit 'movq v2 v1)]
          [,label (emit-label label)])))
    (Program x)))


;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------

(define op->inst
  (lambda (op)
    (case op
      [(+) 'addq]
      [(-) 'subq]
      [(*) 'imulq]
      [(logand) 'andq]
      [(logor) 'orq]
      [(sra) 'sarq]
      [(=) 'je]
      [(<) 'jl]
      [(<=) 'jle]
      [(>) 'jg]
      [(>=) 'jge]
      [else (error who "unexpected binop ~s" op)])))


;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------

(define inst->inst^
				  (lambda (inst)
				    (case inst
				      [(je) 'jne]
				      [(jl) 'jge]
				      [(jle) 'jg]
				      [(jg) 'jle]
				      [(jge) 'jl]
				)))


;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------

(define relop^
	(lambda (op)
		(case op
			['> '<]
			['< '>]
			['<= '>=]
			['= '=])))


;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------

(define enable-optimize-self-reference #f) ;;; set only if optimize-known-call is not called
(define move-bias-enabled #f)

(compiler-passes '(
  parse-scheme
	convert-complex-datum
			uncover-assigned
			purify-letrec
			convert-assignments
			;optimize-direct-call ;;; optimization
			remove-anonymous-lambda
			sanitize-binding-forms
		  uncover-free
			convert-closures
			;optimize-known-call ;;; optimization
			;optimize-self-reference ;;; optimization
			introduce-procedure-primitives
			lift-letrec
			normalize-context
			specify-representation
			uncover-locals
			remove-let
			verify-uil
			remove-complex-opera*
			flatten-set!
			impose-calling-conventions				
			uncover-frame-conflict
			pre-assign-frame
			assign-new-frame
			(iterate
					finalize-frame-locations
					select-instructions
					uncover-register-conflict
					assign-registers
					(break when everybody-home?)
					assign-frame)
			discard-call-live
			finalize-locations
			expose-frame-var
			expose-basic-blocks
			flatten-program
			generate-x86-64
))

