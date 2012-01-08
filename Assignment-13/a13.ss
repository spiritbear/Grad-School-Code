;;; Shiv Indap
;;; sindap
;;; P523
;;; Assignment 13
;;; Andy Keep, Kent Dybvig
;;; P423/P523
;;; Spring 2009

;;; verify-scheme accept a single value and verifies that the value
;;; is a valid program in the current source language.
;;;
;;; The grammar changes only slightly from Assignment 12 in that lambda
;;; expressions may now appear anywhere other expressions may appear.
;;;
;;; Grammar for verify-scheme (assignment 13):
;;;
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
              [(quote ,[Immediate ->]) (values)]
              [(if ,[] ,[] ,[]) (values)]
              [(begin ,[] ... ,[]) (values)]
              [(lambda (,fml* ...) ,x)
               (set! all-uvar* (append fml* all-uvar*))
               ((Expr (append fml* uvar*)) x)]
              [(let ([,new-uvar* ,[]] ...) ,x)
               (set! all-uvar* (append new-uvar* all-uvar*))
               ((Expr (append new-uvar* uvar*)) x)]
              [(letrec ([,new-uvar* (lambda (,fml** ...) ,x*)] ...) ,x)
               (set! all-uvar* (append new-uvar* all-uvar*))
               (let ([uvar* (append new-uvar* uvar*)])
                 (for-each
                   (lambda (fml* x)
                     (set! all-uvar* (append fml* all-uvar*))
                     ((Expr (append fml* uvar*)) x))
                   fml**
                   x*)
                 ((Expr uvar*) x))]
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
      (define (Immediate imm)
        (cond
          [(memq imm '(#t #f ())) (values)]
          [(and (integer? imm) (exact? imm))
           (unless (fixnum-range? imm)
             (error who "integer ~s is out of fixnum range" imm))
           (values)]
          [else (error who "invalid Immediate ~s" imm)]))
      ((Expr '()) x)
      (verify-x-list all-uvar* uvar? 'uvar)))
  (lambda (x) (Program x) x))


;;; ----------------------------------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------------------------------
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
		
;;; ----------------------------------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------------------------------
;;; The main purpose of this pass is to convert all anonymous lambda expressions to letrec expressions
;;;	I implemented this pass by passing an extra parameter to the Expr helper which acts like a flag, when
;;; we encounter an outer lambda expression we change the flag value to 1 and pass it on, if a lambda expression
;;; is encountered anywhere when the flag is 1 it means that it is an anonymous function and the value is reset
;;; to zero, the Lambda helper checks if the flag is set to 1, if it is it returns a new identifier, corresponding
;;; to that anonymous lambda and returns it
	
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


;;; ----------------------------------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------------------------------

;;; The main purpose of this pass is to seperate those uvars that are bound to functions in the let expression
;;; This pass is required since the output-grammar of this pass requires all functions to be only present on
;;; the rhs side of a letrec expression and nowhere else
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
	
;;; The Output Grammar will be as follows	
;;;  Program --> <Expr>
;;;  Expr    --> <uvar>
;;;           |  (quote <Immediate>)
;;;           |  (if <Expr> <Expr> <Expr>)
;;;           |  (begin <Expr>* <Expr>)
;;;           |  (let ([<uvar> <Expr>]*) <Expr>)
;;;           |  (letrec ([<uvar> <Lambda>]*) <Expr>)
;;;           |  (<primitive> <Expr>*)
;;;           |  (<Expr> <Expr>*)
;;;  Lambda  --> (lambda (<uvar>*) <Expr>)
;;;  Immediate -> <fixnum> | () | #t | #f

;;; In the output the <Expr> appearing in the Let cant have a lambda
	
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


;;; ----------------------------------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------------------------------

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

;;; ----------------------------------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------------------------------

;;; Grammar for convert-closures
;;;
;;;  Program --> <Expr>
;;;  Expr   --> <uvar>
;;;          |  (quote <Immediate>)
;;;          |  (if <Expr> <Expr> <Expr>)
;;;          |  (begin <Expr>* <Expr>)
;;;          |  (let ([<uvar> <Expr>]*) <Expr>)
;;;          |  (letrec ([<uvar> (free (<uvar*) (lambda (<uvar>*) <Expr>))]*) <Expr>)
;;;          |  (<primitive> <Expr>*)
;;;          |  (<Expr> <Expr>*)
;;;  Immediate -> <fixnum> | () | #t | #f


;;; This pass firstly passes every lambda expresson with an additional pointer called the closure pointer
;;; this closure pointer basically references to itself., the free-form is changed to bind-free and also adds
;;; the function-pointer to the list, we also have to convert all the uvars bound to function definitions to
;;; labels

;;; We also wraparound the body of a letrec expression with a closures form which is a list mapping
;;; uvars to the labels and the free parameters they take, In my code what I do is that each time I encounter
;;; a label and its corresponding definition I record it and append it to the body-expression I return and
;;; then extract it via extract-closures helper that I have written

;;; Once this transformation has been made, all letrec-bound procedures are closed (have no free variables), 
;;; thus they are simply labeled blocks of code 

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


;;; ----------------------------------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------------------------------

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