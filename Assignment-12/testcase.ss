;(letrec ([fact.0 (lambda (n.3 k.4)
;										(if (eq? n.3 '0) 
;											(k.4 '1)
;											(fact.0 (- n.3 '1) 
;												(letrec ([anon.5 (lambda (v.6)
;																				(k.4 (* n.3 v.6)))])
;												anon.5))))]
;				 [anon.1 (lambda (v.2) v.2)])
;	(fact.0 '5 anon.1))
;	
;	(define-who uncover-free
;		(define primitives
;	  	'(+ - * <= < = >= > procedure? boolean? car cdr cons eq? fixnum? make-vector null? pair? set-car! set-cdr! vector? vector-length vector-ref vector-set! void ))
;		(define Expr
;			(lambda (params)
;				(lambda (expr)
;					(match expr
;						[,uvar (guard (uvar? uvar)) 
;							(if (memq uvar params) (values uvar '()) (values uvar `(,uvar)))]
;						[(if ,[(Expr params) -> cond-expr cond-bindings] ,[(Expr params) -> conseq-expr conseq-bindings] ,[(Expr params) -> alt-expr alt-bindings]) 
;		 					(values `(if ,cond-expr ,conseq-expr ,alt-expr) (append cond-bindings conseq-bindings alt-bindings))]
;						[(quote ,[Immediate -> im-expr]) (values `(quote ,im-expr) '())]
;						[(begin ,[(Expr params)-> exp-stmt* exp-binding*] ... ,[(Expr params) -> tail-stmt tail-binding]) 
;		 					(values `(begin ,exp-stmt* ... ,tail-stmt) '(exp-binding* tail-binding))]
;						[(letrec ([,uvar* ,[Body -> body*]] ...) ,[(Expr params) -> tail-expr tail-binding])
;										(values `(letrec ([,uvar* ,body*] ...) ,tail-expr) '())] 
;						[(let ([,uvar* ,[(Expr params) -> exp* binding*]] ...) ,[(Expr params)-> tail binding]) 
;		 					(values `(let ([,uvar* ,exp*] ...) ,tail) (union binding binding*))]
;						[(,prim ,[(Expr params)-> expr* bindings*] ...) (guard (memq prim primitives))  
;								(values `(,prim ,expr* ...) (apply append bindings*))]
;						[(,[(Expr params) -> exp binding] ,[(Expr params) -> rem* bindings*] ...) 
;						 		(values `(,exp ,rem* ...) (append bindings* binding))]))))
;		 (define Body
;				(lambda (body)
;					(match body
;						[(lambda (,param* ...) ,body) 
;							(let-values ([(stmt* free*) ((Expr param*) body)])
;								`(lambda (,param* ...) (free ,(clean-up free*) ,stmt*)))])))
;		 (define clean-up
;			 (trace-lambda clean(seti)
;				 (cond
;					[(null? seti) '()]
;					[(and (list? (car seti)) (null? (car seti))) (clean-up (cdr seti))]
;					[(list? (car seti)) (set-cons (caar seti) (clean-up (cdr seti)))]
;					[else (set-cons (car seti) (clean-up (cdr seti)))])))
;	   (define (Immediate imm)
;	      (cond
;	        [(memq imm '(#t #f ())) imm]
;	        [(and (integer? imm) (exact? imm))
;	         (unless (fixnum-range? imm)
;	           (error who "integer ~s is out of fixnum range" imm))
;	         imm]
;	        [else (error who "invalid Immediate ~s" imm)]))
;		(lambda (x)
;			(let-values ([(final-expr final-bindings) ((Expr '()) x)])
;				final-expr)))