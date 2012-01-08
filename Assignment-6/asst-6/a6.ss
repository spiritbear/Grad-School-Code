;;;Shiv Indap
;;;sindap
;;;Assignment 6
;;;P523
;;; Andy Keep, Kent Dybvig
;;; P423/P523
;;; Spring 2009

;;;  verify-scheme accepts a signle value and verifies that the value
;;;  is a valid program in the grammar of the sixth assignment.
;;;
;;;  Grammar for verify-scheme (assignment 6):
;;;
;;;  Program --> (letrec ([<label> (lambda (<uvar>*) <Body>)]*) <Body>)
;;;  Body    --> (locals (<uvar>*) <Tail>)
;;;  Tail    --> <Triv>
;;;           |  (binop <Value> <Value>)
;;;           |  (<Value> <Value>*)
;;;           |  (if <Pred> <Tail> <Tail>)
;;;           |  (begin <Effect>* <Tail>)
;;;  Pred    --> (true)
;;;           |  (false)
;;;           |  (<predop> <Value> <Value>)
;;;           |  (if <Pred> <Pred> <Pred>)
;;;           |  (begin <Effect>* <Pred>)
;;;  Effect  --> (nop)
;;;           |  (set! <uvar> <Value>)
;;;           |  (if <Pred> <Effect> <Effect>)
;;;           |  (begin <Effect>* <Effect>)
;;;  Value   --> <Triv>
;;;           |  (<binop> <Value> <Value>)
;;;           |  (if <Pred> <Value> <Value>)
;;;           |  (begin <Effect>* <Value>)
;;;  Triv    --> <uvar> | <int64> | <label>
;;;  
;;;  Where uvar is symbol.n where (n >= 0)
;;;        label is symbol$n where (n >= 0)
;;;        binop is +, -, *, logand, logor, or sra
;;;        predop is <, >, <=, >=, =
;;;
;;;  We still have a couple constraints based on our machine and
;;;  testing framework. Namely, we expect calls target values to
;;;  evaluate to uvars or labels, and we expect computations to be 
;;;  done with uvars or integers.
;;;
;;;  Note that we also expect the sra binop to have a uint6 in the
;;;  second argument.
;;;

;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------

(define-who verify-scheme
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
          [(sra ,[(Value label* uvar*) -> x] ,y)
           (unless (uint6? y)
             (error who "invalid sra operand ~s" y))]
          [(,binop ,[(Value label* uvar*) -> x] ,[(Value label* uvar*) -> y])
           (guard (memq binop '(+ - * logand logor sra)))
           (void)]
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

;;;  Program --> (letrec ([<label> (lambda (<uvar>*) <Body>)]*) <Body>)
;;;  Body    --> (locals (<uvar>*) <Tail>)
;;;  Tail    --> <Triv>
;;;           |  (binop <Value> <Value>)
;;;           |  (<Value> <Value>*)
;;;           |  (if <Pred> <Tail> <Tail>)
;;;           |  (begin <Effect>* <Tail>)
;;;  Pred    --> (true)
;;;           |  (false)
;;;           |  (<predop> <Value> <Value>)
;;;           |  (if <Pred> <Pred> <Pred>)
;;;           |  (begin <Effect>* <Pred>)
;;;  Effect  --> (nop)
;;;           |  (set! <uvar> <Value>)
;;;           |  (if <Pred> <Effect> <Effect>)
;;;           |  (begin <Effect>* <Effect>)
;;;  Value   --> <Triv>
;;;           |  (<binop> <Value> <Value>)
;;;           |  (if <Pred> <Value> <Value>)
;;;           |  (begin <Effect>* <Value>)
;;;  Triv    --> <uvar> | <int64> | <label>


(define-who remove-complex-opera*
	(define (Body bd)
	  (define new-local* '())
	  (define (new-t)
	    (let ([t (unique-name 't)])
	      (set! new-local* (cons t new-local*))
	      t))
	  (define trivial?   ;;Checks wether a expression is trivial or not
			(lambda (exp)
				(if (or (uvar? exp) (label? exp) (int64? exp)) #t #f)))
		(define non-trivial? 
			(lambda (exp)
				(not (trivial? exp))))
		(define handle-operands				
			(lambda (op rand1 rand2)
				(cond
					[(and (trivial? rand1) (trivial? rand2)) `(,op ,rand1 ,rand2)] ;;if operands trivial return expression as is
					[(trivial? rand1)
						(let ((rand-b (new-t)))
							(make-begin `((set! ,rand-b ,rand2) (,op ,rand1 ,rand-b))))]
					[(trivial? rand2)
						(let ((rand-a (new-t)))
							(make-begin `((set! ,rand-a ,rand1) (,op ,rand-a ,rand2))))]
					[else
						(let ((rand-a (new-t)) (rand-b (new-t)))											;;if operands non-trivial (f$1 (+ (* x.1 y.2) (* x.3 y.4)))
							(make-begin `((set! ,rand-a ,rand1) (set! ,rand-b ,rand2) (,op ,rand-a ,rand-b))))])))
	  (define Triv
			(lambda (triv)
				triv))
	  (define Value
			(lambda (val)
				(match val
					[(begin ,[Effect -> ef*] ... , [Value -> val]) `(begin ,ef* ... ,val)]
					[(if ,[Pred -> pred] ,[Value -> conseq] ,[Value -> alt]) `(if ,pred ,conseq ,alt)]
					[(,binop ,[Value -> rand1] ,[Value -> rand2]) (handle-operands binop rand1 rand2)]
					[,triv triv])))
	  (define Effect
			(lambda (effect)
				(match effect
					[(nop) '(nop)]
					[(set! ,unique-var ,[Value -> value]) `(set! ,unique-var ,value)]
					[(if ,[Pred -> pred] ,[Effect -> conseq] ,[Effect -> alt]) `(if ,pred ,conseq ,alt)]
					[(begin ,[Effect -> effect*] ...) `(begin ,effect* ...)])))
	  (define Pred
			(lambda (pred)
				(match pred
					[(true) '(true)]
					[(false) '(false)]
					[(if ,[Pred -> conde] ,[Pred -> conseq] ,[Pred -> alt]) `(if ,conde ,conseq ,alt)]
					[(begin ,[Effect -> effect*] ... ,[Pred -> tail]) `(begin ,effect* ... ,tail)]
					[(,relop ,[Value -> rand1] ,[Value -> rand2]) (handle-operands relop rand1 rand2)])))
		(define extract-vars
			(lambda (tail-expr)
				(match tail-expr
					[(,stmt* ...,tail) (if (trivial? tail) tail)]
					[,triv triv])))
		(define extract-stmts
			(lambda (tail-expr)
				(match tail-expr
					[(,stmt* ...,tail) stmt*]
					[,triv '()])))	
		(define make-trivial			;;Makes the arguments trivial 
			(lambda (arg)
				(if (trivial? arg) arg
					(let ((new-var (new-t)))
						`((set! ,new-var ,arg) ,new-var)))))
		(define make-tail
			(lambda (stmts vars)
				(make-begin (append (apply append stmts) (list vars)))))
	  (define Tail
			(lambda (tail)
				(match tail
					[(begin ,[Effect -> ef*] ... , [Tail -> tail-exp]) `(begin ,ef* ... ,tail-exp)]
					[(if ,[Pred -> pred] ,[Tail -> conseq] , [Tail -> alt]) `(if ,pred ,conseq ,alt)]
					[(,binop ,[Value -> rand1] ,[Value -> rand2]) (guard (memq binop '(+ - * logand logor sra))) (handle-operands binop rand1 rand2)]
					[(,[Value -> value] ,[Value -> value*] ...)
					 	(let* ((trivial-tail (map make-trivial `(,value ,value* ...)))
									 (vars (map extract-vars trivial-tail))
									 (stmts (map extract-stmts trivial-tail)))
									(make-tail stmts vars))]  ;;Makes the tail expression all statements come before all newly introduced temporaries are passed as arguments
					[,[Triv -> triv] triv])))			
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

;;;Program	----->	(letrec ([label (lambda (uvar*) Body)]*) Body)
;;;Body	----->	(locals (uvar*) Tail)
;;;Tail	----->	Triv
;;;	|	(binop Triv Triv)
;;;	|	(Triv Triv*)
;;;	|	(if Pred Tail Tail)
;;;	|	(begin Effect* Tail)
;;;Pred	----->	(true)
;;;	|	(false)
;;;	|	(relop Triv Triv)
;;;	|	(if Pred Pred Pred)
;;;	|	(begin Effect* Pred)
;;;Effect	----->	(nop)
;;;	|	(set! uvar Value)
;;;	|	(if Pred Effect Effect)
;;;	|	(begin Effect* Effect)
;;;Value	----->	Triv
;;;	|	(binop Triv Triv)
;;;	|	(if Pred Value Value)
;;;	|	(begin Effect* Value)
;;;Triv	----->	uvar | int | label


(define-who flatten-set!
	(define make-flatten			;;This function will be called when there is (set! x.1 complex-expression) encountered in Effect we push down the x.1 in begin expression
		(lambda (var expr) 
			(match expr
				[(begin ,[Effect -> ef*] ... ,val) `(begin ,ef* ... (set! ,var ,val))]
				[(if ,[Pred -> pred] ,conseq ,alt) `(if ,pred ,(make-flatten var conseq) ,(make-flatten var alt))]
				[(,binop ,rand1 ,rand2) `(set! ,var ,expr)]
				[,x `(set! ,var ,x)])))
  (define Triv
		(lambda (triv)
			triv))
  (define Effect
		(lambda (effect)
			(match effect
				[(nop) '(nop)]
				[(set! ,unique-var ,expr) (make-flatten unique-var expr)]
				[(if ,[Pred -> pred] ,[Effect -> conseq] ,[Effect -> alt]) `(if ,pred ,conseq ,alt)]
				[(begin ,[Effect -> effect*] ...) `(begin ,effect* ...)])))
  (define Pred
		(lambda (pred)
			(match pred
				[(true) '(true)]
				[(false) '(false)]
				[(if ,[Pred -> conde] ,[Pred -> conseq] ,[Pred -> alt]) `(if ,conde ,conseq ,alt)]
				[(begin ,[Effect -> effect*] ... ,[Pred -> tail])  (make-begin `(,effect* ... ,tail))]
				[(,relop ,rand1 ,rand2) pred])))
  (define Tail
		(lambda (tail)
			(match tail
				[(begin ,[Effect -> ef*] ... , [Tail -> tail-exp]) (make-begin `(,ef* ... ,tail-exp))]
				[(if ,[Pred -> pred] ,[Tail -> conseq] , [Tail -> alt]) `(if ,pred ,conseq ,alt)]
				[,triv triv])))
  (define (Body bd)
    (match bd
      [(locals (,uvar* ...) ,[Tail -> tail]) `(locals (,uvar* ...) ,tail)]
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
;;;	|	(if Pred Effect Effect)
;;;	|	(begin Effect* Effect)
;;;Triv	----->	uvar | int | label

(define-who impose-calling-conventions
	(define store-return-value
		(lambda (tail)
			`(set! ,return-value-register ,tail)))
	(define find-max				;;Function required to assign frame-variables to formal parameters
		(lambda (ls)
			(cond
				[(null? (cdr ls)) (car ls)]
				[else (max (car ls) (find-max (cdr ls)))])))
	(define assigned-registers '())
	(define assigned-frame-locations '())
	(define assign-parameters ;;assigns registers and frame-locations to formal parameters
		(lambda (val)
			(cond
				[(< (length assigned-registers) (length parameter-registers)) 
						(let ((assignment (car (difference parameter-registers assigned-registers))))
							 (set! assigned-registers (cons assignment assigned-registers))
							`(set! ,val ,assignment))]
				[(null? assigned-frame-locations)
					(let ((assignment 'fv0))
						 (set! assigned-frame-locations (cons assignment assigned-frame-locations))
						`(set! ,val ,assignment))]
				[else (let* ((max-val (find-max (map (lambda (z) (if (frame-var? z) (frame-var->index z) '-1)) assigned-frame-locations))) 
										(assignment (index->frame-var (+ 1 max-val))))
								 (set! assigned-frame-locations (cons assignment assigned-frame-locations))
								`(set! ,val ,assignment))])))
	(define assign-parameters^
		(lambda (val)
			(cond
				[(< (length assigned-registers) (length parameter-registers)) 
						(let ((assignment (car (difference parameter-registers assigned-registers))))
							 (set! assigned-registers (cons assignment assigned-registers))
							`(set! ,assignment ,val))]
				[(null? assigned-frame-locations)
					(let ((assignment 'fv0))
						 (set! assigned-frame-locations (cons assignment assigned-frame-locations))
						`(set! ,assignment ,val))]
				[else (let* ((max-val (find-max (map (lambda (z) (if (frame-var? z) (frame-var->index z) '-1)) assigned-frame-locations))) 
										(assignment (index->frame-var (+ 1 max-val))))
								 (set! assigned-frame-locations (cons assignment assigned-frame-locations))
								`(set! ,assignment ,val))])))
	(define make-flatten
			(lambda (var expr)
				(match expr
					[(begin ,[Effect -> ef*] ... ,val) `(begin ,ef* ... (set! ,var ,val))]
					[(if ,[Pred -> pred] ,conseq ,alt) `(if ,pred ,(make-flatten var conseq) ,(make-flatten var alt))]
					[(,binop ,rand1 ,rand2) `(set! ,var ,expr)]
					[,x `(set! ,var ,x)])))
  (define Triv
		(lambda (triv)
			triv))
  (define Effect
		(lambda (effect)
			(match effect
				[(nop) '(nop)]
				[(set! ,unique-var ,expr) (make-flatten unique-var expr)]
				[(if ,[Pred -> pred] ,[Effect -> conseq] ,[Effect -> alt]) `(if ,pred ,conseq ,alt)]
				[(begin ,[Effect -> effect*] ...) `(begin ,effect* ...)])))
  (define Pred
		(lambda (pred)
			(match pred
				[(true) '(true)]
				[(false) '(false)]
				[(if ,[Pred -> conde] ,[Pred -> conseq] ,[Pred -> alt]) `(if ,conde ,conseq ,alt)]
				[(begin ,[Effect -> effect*] ... ,[Pred -> tail]) `(begin ,effect* ... ,tail)]
				[(,relop ,rand1 ,rand2) pred])))
  (define Tail
		(lambda (tail rp)
			(match tail
				[(begin ,[Effect -> ef*] ... , tail-exp)
					(let ((expression (Tail tail-exp rp)))
								(make-begin `(,ef* ... ,expression)))]
				[(if ,[Pred -> pred] ,conseq ,alt) 
					(let ((conseq-expr (Tail conseq rp))
								(alt-expr (Tail alt rp)))
						`(if ,pred ,conseq-expr ,alt-expr))]
				[(,op ,rand1 ,rand2) (guard (memq op '(+ - * logand logor sra)))
								(let ((return-value-expr (store-return-value tail))
									   (return-calling-expr `(,rp ,frame-pointer-register ,return-value-register)))
											`(begin ,return-value-expr ,return-calling-expr))]
				[(,triv ,loc* ...)
						(let ((formal-assignments (map assign-parameters^ loc*)) 
									(return-exp `(set! ,return-address-register ,rp))
									(return-registers (append (list return-address-register frame-pointer-register) (union assigned-registers assigned-frame-locations))))
										(set! assigned-registers '())
										(set! assigned-frame-locations '())									
									`(begin ,@formal-assignments ,return-exp ,(cons triv return-registers)))]
				[,triv 
					(let ((return-value-expr (store-return-value tail))
						   (return-calling-expr `(,rp ,frame-pointer-register ,return-value-register)))
								`(begin ,return-value-expr ,return-calling-expr))])))		
  (define (Body bd fml*) ;fml* is holding all the formal prameters for which a home must be found
    (match bd
      [(locals (,locals* ...) ,tail)
				(let* ((return-var (unique-name 'rp)) ;Storing the return address
							(formal-assignments (map assign-parameters fml*)) ;;assign all parameters to registers or frame variables
							(begin-expr (cons `(set! ,return-var ,return-address-register) formal-assignments)) ;;add the (set! rp return-reg) in front
							(tail-expr (make-begin `(,@begin-expr ,tail))))	;;make a begin expression with the existing tail appended
							(set! assigned-registers '())										;;keeping track of registers assigned to parameters has been done using side effects
							(set! assigned-frame-locations '())							;;keeping track of frames assigned to parameters has been done using side effects
					`(locals (,locals* ... ,return-var ,fml* ...) ,(Tail tail-expr return-var)))]
      [,bd (error who "invalid Body ~s" bd)]))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda (,fml** ...) ,bd*)] ...) ,bd)
       (let ([bd* (map Body bd* fml**)] [bd (Body bd '())])
         `(letrec ([,label* (lambda () ,bd*)] ...) ,bd))]
      [,x (error who "invalid Program ~s" x)])))

;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------------------

