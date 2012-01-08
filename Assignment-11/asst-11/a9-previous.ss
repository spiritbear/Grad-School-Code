;;; Shiv Indap
;;; sindap
;;; Assignment-9
;;; Andy Keep, Kent Dybvig
;;; P423/P523
;;; Spring 2009

;;; verify-scheme accept a single value and verifies that the value
;;; is a valid program in the grammar of the ninth assignment.
;;;
;;; Grammar for verify-scheme (assignment 8):
;;;
;;;  Program --> (letrec ([<label> (lambda (<uvar>*) <Tail>)]*) <Tail>)
;;;  Tail    --> <Triv>
;;;           |  (binop <Value> <Value>)
;;;           |  (alloc <Value>)
;;;           |  (<Value> <Value>*)
;;;           |  (if <Pred> <Tail> <Tail>)
;;;           |  (begin <Effect>* <Tail>)
;;;           |  (let ([<uvar> <Value>]*) <Tail>)
;;;  Pred    --> (true)
;;;           |  (false)
;;;           |  (<relop> <Value> <Value>)
;;;           |  (if <Pred> <Pred> <Pred>)
;;;           |  (begin <Effect>* <Pred>)
;;;           |  (let ([<uvar> <Value>]*) <Pred>)
;;;  Effect  --> (nop)
;;;           |  (mset! <Value> <Value> <Value>)
;;;           |  (<Value> <Value>*)
;;;           |  (if <Pred> <Effect> <Effect>)
;;;           |  (begin <Effect>* <Effect>)
;;;           |  (let ([<uvar> <Value>]*) <Effect>)
;;;  Value   --> <Triv>
;;;           |  (<binop> <Value> <Value>)
;;;           |  (alloc <Value>)
;;;           |  (<Value> <Value>*)
;;;           |  (if <Pred> <Value> <Value>)
;;;           |  (begin <Effect>* <Value>)
;;;           |  (let ([<uvar> <Value>]*) <Value>)
;;;  Triv    --> <uvar> | <integer> | <label>
;;;
;;; Where uvar is symbol.n, n >= 0
;;;       binop is mref +, -, *, logand, logor, or sra
;;;       relop is <, <=, =, >=, >
;;;       label is symbol$n, n >= 0
;;;
;;; Each label bound by the letrec expression must have a unique suffix,
;;; and each uvar bound by a lambda or let expression must have a unique
;;; suffix, within the same Program.
;;;
;;; Machine constraints:
;;;   - sra's second operand must be an exact integer k, 0 <= k <= 63
;;;   - each other integer must be a exact integer n, -2^63 <= n <= 2^63-1
;;;
;;; If the value is a valid program, verify-scheme returns the value
;;; unchanged; otherwise it signals an error.


;;; uncover-locals simply gets the list of variables in let expressions
;;; Since we donot change the Tail expression we can return it as it is and simply wrap around a locals form
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



;;;(let ([x.3 (+ x.3 3)]
;;;			[y.4 (* y.4 2)]))
;;; will get converted to (begin (set! x.3 (+ x.3 3)) (set! y.4 (* y.4 2)))
;;; I have created an anonymous function that achieves this 
;;; It binds the variable to its corresponding expressions			
			
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
