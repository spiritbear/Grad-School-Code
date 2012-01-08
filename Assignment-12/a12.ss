;;; Shiv Indap
;;; sindap
;;; P523
;;; Andy Keep, Kent Dybvig
;;; P423/P523
;;; Spring 2009

;;; verify-scheme accept a single value and verifies that the value
;;; is a valid program in the current source language.
;;;
;;; The grammar changes only slightly from Assignment 11 in that labels
;;; no longer appear in the source language.  Also, the set of variables
;;; visible within each lambda expression now includes those bound by
;;; let, letrec, and lambda expressions enclosing the lambda expression.
;;;
;;; Grammar for verify-scheme (assignment 12):
;;;
;;;  Program --> <Expr>
;;;  Expr   --> <uvar>
;;;          |  (quote <Immediate>)
;;;          |  (if <Expr> <Expr> <Expr>)
;;;          |  (begin <Expr>* <Expr>)
;;;          |  (let ([<uvar> <Expr>]*) <Expr>)
;;;          |  (letrec ([<uvar> (lambda (<uvar>*) <Expr>)]*) <Expr>)
;;;          |  (<primitive> <Expr>*)
;;;          |  (<Expr> <Expr>*)
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

;;; Uncover free basically finds the variables that are free in a lambda body and just wraps around a new
;;; form free(var*)(lambda(args*) ....) as shown, the startegy I have adopted is relatively simple, I simply
;;; accumulate all variable refernces made in a lambda body starting bottom-up, the moment I encounter
;;; a lambda exp I simply find the difference of that list of variables and param*

;;; All helpers here return 2 values , the 1st being the expression itself and the next being all the variables
;;; in the expression

;;; Wherever the cataform returns a list of lists I have used apply append to splice the lists
;;; Body Expressions are handled seperately and involve calling Expr on the Body, I did this only so that
;;; I could wrap around the free forms round the lambda, body also returns 2 values, the new body exp and the list
;;; of free variables in a lambda expression

;;; clean-up function just ensures that our list of free variables contains no duplicates null-lists and so
;;; on

;;; Grammar for uncover-free 
;;;
;;;  Program --> <Expr>
;;;  Expr   --> <uvar>
;;;          |  (quote <Immediate>)
;;;          |  (if <Expr> <Expr> <Expr>)
;;;          |  (begin <Expr>* <Expr>)
;;;          |  (let ([<uvar> <Expr>]*) <Expr>)
;;;          |  (letrec ([<uvar> (lambda (<uvar>*) <Expr>)]*) <Expr>)
;;;          |  (<primitive> <Expr>*)
;;;          |  (<Expr> <Expr>*)
;;;  Immediate -> <fixnum> | () | #t | #f

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

;;; Grammar for procedure-primitives
;;;
;;;  Program --> <Expr>
;;;  Expr   --> <uvar>
;;;          |  (quote <Immediate>)
;;;          |  (if <Expr> <Expr> <Expr>)
;;;          |  (begin <Expr>* <Expr>)
;;;          |  (let ([<uvar> <Expr>]*) <Expr>)
;;;          |  (letrec ([<uvar> (bind-free (<uvar*) (lambda (<uvar>*) (closures (closure*) <Expr>)))]*) <Expr>)
;;;          |  (<primitive> <Expr>*)
;;;          |  (<Expr> <Expr>*)
;;;  Immediate -> <fixnum> | () | #t | #f

;;; In This pass Expr takes 2 arguments the first being a function pointer and the next being a list of free 
;;; variables, these are necessary because we need to generate the procedure-code procedure-ref procedure-set!
;;; if in expr expr* context we fnd that the free-list is null we simply copy the 1st argument and make a procedure
;;; -code of the first argument else we know that this argument is in our list of parameters and we need to do
;;; a procedure-ref to it so we  have to find the appropriate value in the record 
;;; and pass along the rest of the arguments

;;; I also have seperate Body and Tail helpers to deal with the bind-free and closures forms repectively


;----------------- --------------- ---------- ---------- ---------- ----------



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
		 (trace-lambda Tail(tail)
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
			(trace-lambda Body(body)
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
		
;;; ----------------------------------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------------------------------

;;; Here I added the new primitives that we may encounter related to the procedure, The remaining code should
;;; remain unchanged

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

;;; ----------------------------------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------------------------------
;;; The only change I made in This pass was 
;;; added make-procedure procedure-ref procedure-code in the possible primitives in Value Context
;;; added procedure-set! in Effect context
;;; added procedure? in Predicate Context

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
		(trace-lambda Pred(pred)
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

;;----------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------

;;; Simply removes (nop) in begin statements
;;; Taken from the code provided in the Assignment Description
(define (make-nopless-begin x*)
  (let ([x* (remove '(nop) x*)])
    (if (null? x*)
        '(nop)
        (make-begin x*))))

;;----------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------


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
