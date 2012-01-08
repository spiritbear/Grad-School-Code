;;; Shiv Indap
;;; sindap
;;; P523
;;; Assignment-14
;;; Andy Keep, Kent Dybvig
;;; P423/P523
;;; Spring 2009

;;; verify-scheme accept a single value and verifies that the value
;;; is a valid program in the current source language.
;;;
;;; The grammar changes from Assignment 13 in that letrec right-hand
;;; sides can now be arbitrary expressions and in the reintroduction
;;; of set! expressions.
;;;
;;; Grammar for verify-scheme (assignment 14):
;;;
;;;  Program --> <Expr>
;;;  Expr    --> <uvar>
;;;           |  (quote <Immediate>)
;;;           |  (if <Expr> <Expr> <Expr>)
;;;           |  (begin <Expr>* <Expr>)
;;;           |  (lambda (<uvar>*) <Expr>)
;;;           |  (let ([<uvar> <Expr>]*) <Expr>)
;;;           |  (letrec ([<uvar> <Expr>]*) <Expr>)
;;;           |  (set! <uvar> <Expr>)
;;;           |  (<primitive> <Expr>*)
;;;           |  (<Expr> <Expr>*)
;;;  Immediate -> <fixnum> | () | #t | #f
;;;
;;; Where uvar is symbol.n, n >= 0
;;;       fixnum is an exact integer
;;;       primitives are void (zero arguments); car, cdr, vector-length,
;;;         make-vector, boolean?, fixnum?, null?, pair?, procedure?,
;;;         vector? (one argument); *, +, -, cons, vector-ref, <, <=, =,
;;;         >=, >, eq?, set-car!, set-cdr! (two arguments); and vector-set!
;;;         (three arguments).
;;;
;;; Within the same Program, each uvar bound by a lambda, let, or letrec
;;; expression must have a unique suffix.
;;;
;;; Machine constraints:
;;;   - each fixnum must be an exact integer n, -2^(k-1) <= n <= 2^(k-1)-1,
;;;     where k is the value of the helpers.ss variable fixnum-bits
;;;
;;; If the value is a valid program, verify-scheme returns the value
;;; unchanged; otherwise it signals an error.

(define-who verify-scheme
  (define primitives
    '((+ . 2) (- . 2) (* . 2) (<= . 2) (< . 2) (= . 2)
      (>= . 2) (> . 2) (boolean? . 1) (car . 1) (cdr . 1)
      (cons . 2) (eq? . 2) (fixnum? . 1) (make-vector . 1)
      (null? . 1) (pair? . 1) (procedure? . 1) (set-car! . 2)
      (set-cdr! . 2) (vector? . 1) (vector-length . 1)
      (vector-ref . 2) (vector-set! . 3) (void . 0)))
  (define (datum? x)
    (define (constant? x)
      (or (memq x '(#t #f ()))
          (and (and (integer? x) (exact? x))
               (or (fixnum-range? x)
                   (error who "integer ~s is out of fixnum range" x)))))
    (or (constant? x)
        (if (pair? x)
            (and (datum? (car x)) (datum? (cdr x)))
            (and (vector? x) (andmap datum? (vector->list x))))))
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
  (define Program
    (lambda (x)
      (define all-uvar* '())
      (define Expr
        (lambda (uvar*)
          (lambda (x)
            (match x
              [,uvar (guard (uvar? uvar))
               (if (memq uvar uvar*)
                   (values)
                   (error who "unbound uvar ~s" uvar))]
              [(quote ,x)
               (unless (datum? x) (error who "invalid datum ~s" x))
               (values)]
              [(if ,[] ,[] ,[]) (values)]
              [(begin ,[] ... ,[]) (values)]
              [(lambda (,fml* ...) ,x)
               (set! all-uvar* (append fml* all-uvar*))
               ((Expr (append fml* uvar*)) x)]
              [(let ([,new-uvar* ,[]] ...) ,x)
               (set! all-uvar* (append new-uvar* all-uvar*))
               ((Expr (append new-uvar* uvar*)) x)]
              [(letrec ([,new-uvar* ,rhs*] ...) ,x)
               (set! all-uvar* (append new-uvar* all-uvar*))
               (let ([p (Expr (append new-uvar* uvar*))])
                 (for-each p rhs*)
                 (p x))]
              [(set! ,uvar ,[])
               (unless (uvar? uvar) (error who "invalid set! lhs ~s" uvar))
               (if (memq uvar uvar*)
                   (values)
                   (error who "unbound uvar ~s" uvar))]
              [(,prim ,x* ...)
               (guard (assq prim primitives))
               (unless (= (length x*) (cdr (assq prim primitives)))
                 (error who "too many or few arguments ~s for ~s" (length x*) prim))
               (for-each (Expr uvar*) x*)
               (values)]
              [(,x ,y ...)
               (guard (and (symbol? x) (not (uvar? x))))
               (error who "invalid Expr ~s" `(,x ,y ...))]
              [(,[] ,[] ...) (values)]
              [,x (error who "invalid Expr ~s" x)]))))
      ((Expr '()) x)
      (verify-x-list all-uvar* uvar? 'uvar)))
  (lambda (x) (Program x) x))

;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------

;;; Grammar for convert-complex-datum (assignment 14):
;;;
;;;  Program --> <Expr>
;;;  Expr    --> <uvar>
;;;           |  (quote <Immediate>)
;;;           |  (if <Expr> <Expr> <Expr>)
;;;           |  (begin <Expr>* <Expr>)
;;;           |  (lambda (<uvar>*) <Expr>)
;;;           |  (let ([<uvar> <Expr>]*) <Expr>)
;;;           |  (letrec ([<uvar> <Expr>]*) <Expr>)
;;;           |  (set! <uvar> <Expr>)
;;;           |  (<primitive> <Expr>*)
;;;           |  (<Expr> <Expr>*)
;;;  Immediate -> <fixnum> | () | #t | #f | <vector> | <list>

;;; The strategy here is to convert a complex datatype like list or vector into its equivlent primitive code
;;; e.g '(1 2 3) will be converted to (cons '1 (cons '2 (cons '3 '())))
;;; The Expr and datum helpers return 2 values 
;;; This is useful because i we encounter a list like '(1 2 3)
;;; we simply replace it with an uvar-binding and add the uvar-binding at the top-level using a let and process it later


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
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
								
;;;  Program --> <Expr>
;;;  Expr    --> <uvar>
;;;           |  (quote <Immediate>)
;;;           |  (if <Expr> <Expr> <Expr>)
;;;           |  (begin <Expr>* <Expr>)
;;;           |  (lambda (<uvar>*) <Expr>)
;;;           |  (let ([<uvar> <Expr>]*) <Expr>)
;;;           |  (letrec ([<uvar> <Expr>]*) <Expr>)
;;;           |  (set! <uvar> <Expr>)
;;;           |  (<primitive> <Expr>*)
;;;           |  (<Expr> <Expr>*)
;;;  Immediate -> <fixnum> | () | #t | #f 

;;; uncover-assigned simply traverses through each of the expressions and records the uvars which occur on the lhs of a set!
;;; expression, when we come to a top-level expression that lambda,letrrec or let we simply add a new assigned form below these declarations to indicate
;;; that these variables have been set!, consequently we also pass on the list of uvars to the enclosing expression and record the uvars where set! has
;;; been used

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
;;; ------------------------------------------------------------------------------------------------------------------------------------------------

;;;  Program --> <Expr>
;;;  Expr    --> <uvar>
;;;           |  (quote <Immediate>)
;;;           |  (if <Expr> <Expr> <Expr>)
;;;           |  (begin <Expr>* <Expr>)
;;;           |  (lambda (<uvar>*) (assigned (assign*) <Expr>))
;;;           |  (let ([<uvar> <Expr>]*) (assigned (assign*) <Expr>))
;;;           |  (letrec ([<uvar> <Expr>]*) (assigned (assign*) <Expr>))
;;;           |  (set! <uvar> <Expr>)
;;;           |  (<primitive> <Expr>*)
;;;           |  (<Expr> <Expr>*)
;;;  Immediate -> <fixnum> | () | #t | #f

;;; This pass rewrites letrec expressions so that, in the output of the pass, all letrec expressions are "pure," i.e., bind only unassigned 
;;; variables to lambda expressions. 
;;; In a series of letrec bindings if even one of them is impure then we convert everything to a let-form
;;;(letrec ((x e) ...)
;;;  (assigned (x! ...)
;;;    body)) ->
;;;  (let ((x (void)) ...)
;;;    (assigned (x ...)
;;;      (begin
;;;        (let ((t e) ...)
;;;          (assigned ()
;;;            (begin (set! x t) ...)))
;;;        body)))

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
	;;; traverses over the exps bound by letrec to see if they all are pure, if they are then true else false
	(define seperate-lambdas
		(lambda (assign* uvar* exp*)
			(cond
				[(and (null? assign*) (null? uvar*)) #t]
				;;;Lambda Expression
				[(and (null? assign*) (lambda-expr? (car exp*))) (seperate-lambdas assign* (cdr uvar*) (cdr exp*))]
				[else #f])))
	;;; generates the set! in the inner-let expression
	(define generate-set!
		(lambda (x y)
				`(set! ,y ,x)))
	;;x.5 will be converted into a new unique also beginning with x
	(define generate-uvar
		(lambda (uvar)
			(unique-name (string->symbol (extract-root uvar)))))
	(lambda (x)
		(Expr x)))
		
		
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------
;;; ------------------------------------------------------------------------------------------------------------------------------------------------

;;;  Program --> <Expr>
;;;  Expr    --> <uvar>
;;;           |  (quote <Immediate>)
;;;           |  (if <Expr> <Expr> <Expr>)
;;;           |  (begin <Expr>* <Expr>)
;;;           |  (lambda (<uvar>*) (assigned (assign*) <Expr>))
;;;           |  (let ([<uvar> <Expr>]*) (assigned (assign*) <Expr>))
;;;           |  (letrec ([<uvar> <Expr>]*) (assigned (assign*) <Expr>))
;;;           |  (set! <uvar> <Expr>)
;;;           |  (<primitive> <Expr>*)
;;;           |  (<Expr> <Expr>*)
;;;  Immediate -> <fixnum> | () | #t | #f

;;; This pass gets rid of the set! form, every expresson of the form (set! x rhs)
;;; is replaced by (set-car! x rhs), since x is converted to a pair in which the car holds the value and the cdr is void
;;; in addition to that those uvars that find themselves in the assigned form are replaced with the (cons new-var (void))
;;; where new-var is the expression e initially bound to the old expression
;;; Expr helper takes the list of uvars that have been assigned as well

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