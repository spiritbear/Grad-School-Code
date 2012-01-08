;;; Andy Keep, Kent Dybvig
;;; P423/P523
;;; Spring 2009

;;; verify-scheme accept a single value and verifies that the value
;;; is a valid program in the grammar of the ninth assignment.
;;;
;;; Grammar for verify-scheme (assignment 10):
;;;
;;;  Program --> (letrec ([<label> (lambda (<uvar>*) <Value>)]*) <Value>)
;;;  Value   --> <label>
;;;           |  <uvar>
;;;           |  (quote <Immediate>)
;;;           |  (if <Pred> <Value> <Value>)
;;;           |  (begin <Effect>* <Value>)
;;;           |  (let ([<uvar> <Value>]*) <Value>)
;;;           |  (<value-prim> <Value>*)
;;;           |  (<Value> <Value>*)
;;;  Pred    --> (true)
;;;           |  (false)
;;;           |  (if <Pred> <Pred> <Pred>)
;;;           |  (begin <Effect>* <Pred>)
;;;           |  (let ([<uvar> <Value>]*) <Pred>)
;;;           |  (<pred-prim> <Value>*)
;;;  Effect  --> (nop)
;;;           |  (if <Pred> <Effect> <Effect>)
;;;           |  (begin <Effect>* <Effect>)
;;;           |  (let ([<uvar> <Value>]*) <Effect>)
;;;           |  (<effect-prim> <Value>*)
;;;           |  (<Value> <Value>*)
;;;  Immediate -> <fixnum> | () | #t | #f
;;;
;;; Where uvar is symbol.n, n >= 0
;;;       label is symbol$n, n >= 0
;;;       fixnum is an exact integer
;;;       value-prims are void (zero arguments); car, cdr, vector-length,
;;;         make-vector (one argument); *, +, -, cons, vector-ref
;;;         (two arguments)
;;;       pred-prims are boolean?, fixnum?, null?, pair?, vector?
;;;         (one argument); <, <=, =, >=, >, eq? (two arguments)
;;;       effect-prims are set-car!, set-cdr! (two arguments);
;;;         vector-set! (three arguments)
;;;
;;; Each label bound by the letrec expression must have a unique suffix,
;;; and each uvar bound by a lambda or let expression must have a unique
;;; suffix, within the same Program.
;;;
;;; Machine constraints:
;;;   - each fixnum must be an exact integer n, -2^(k-1) <= n <= 2^(k-1)-1,
;;;     where k is the value of the helpers.ss variable fixnum-bits
;;;
;;; If the value is a valid program, verify-scheme returns the value
;;; unchanged; otherwise it signals an error.

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
				[(eq? rator 'vector-length)
					`(mref ,(car rand*) ,(- disp-vector-length tag-vector))]
				[(eq? rator 'vector-ref)
					(let ([value (cadr rand*)])
						(if (integer? value) 
								`(mref ,(car rand*) ,(+ (- disp-vector-data tag-vector) value))
								`(mref ,(car rand*) (+ ,(- disp-vector-data tag-vector) ,value))))]
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
				 				`(mset! ,(car rand*) (+ ,(- disp-vector-data tag-vector) ,value) ,(caddr rand*))))])))
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
					`(= (logand ,(car rand*) ,mask-pair) ,tag-pair)])))
	(define val-primitive?
		(lambda (x)
			(memq x '(+ - car cdr cons make-vector vector-length vector-ref ))))
	(define effect-primitive?
		(lambda (x)
			(memq x '(set-car! set-cdr! vector-set!))))
	(define predicate-primitive?
		(lambda (x)
			(memq x '(<= < > = >= boolean? eq? fixnum? null? pair? vector?))))
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