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

(define-who verify-scheme
  (define value-prims
    '((* . 2) (+ . 2) (- . 2) (car . 1) (cdr . 1) (cons . 2)
       (make-vector . 1) (vector-length . 1) (vector-ref . 2)
       (void . 0)))
  (define pred-prims
    '((< . 2) (<= . 2) (= . 2) (>= . 2) (> . 2) (boolean? . 1)
       (eq? . 2) (fixnum? . 1) (null? . 1) (pair? . 1)
       (vector? . 1)))
  (define effect-prims '((set-car! . 2) (set-cdr! . 2) (vector-set! . 3)))
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
      (define Body
        (lambda (label* fml*)
          (define (Immediate imm)
            (cond
              [(memq imm '(#t #f ())) (values)]
              [(and (integer? imm) (exact? imm))
               (unless (fixnum-range? imm)
                 (error who "integer ~s is out of fixnum range" imm))
               (values)]
              [else (error who "invalid Immediate ~s" imm)]))
          (define Value
            (lambda (uvar*)
              (lambda (val)
                (match val
                  [,label (guard (label? label))
                   (if (memq label label*)
                       (values)
                       (error who "unbound label ~s" label))]
                  [,uvar (guard (uvar? uvar))
                   (if (memq uvar uvar*)
                       (values)
                       (error who "unbound uvar ~s" uvar))]
                  [(quote ,[Immediate ->]) (values)]
                  [(if ,[(Pred uvar*) ->] ,[] ,[]) (values)]
                  [(begin ,[(Effect uvar*) ->] ... ,[]) (values)]
                  [(let ([,new-uvar* ,[]] ...) ,val)
                   (set! all-uvar* (append new-uvar* all-uvar*))
                   ((Value (append new-uvar* uvar*)) val)]
                  [(,prim ,x* ...)
                   (guard (assq prim value-prims))
                   (unless (= (length x*) (cdr (assq prim value-prims)))
                     (error who "too many or few arguments ~s for ~s" (length x*) prim))
                   (for-each (Value uvar*) x*)
                   (values)]
                  [(,x ,y ...)
                   (guard (and (symbol? x) (not (or (uvar? x) (label? x)))))
                   (error who "invalid Value ~s" `(,x ,y ...))]
                  [(,[] ,[] ...) (values)]
                  [,val (error who "invalid Value ~s" val)]))))
          (define Effect
            (lambda (uvar*)
              (lambda (ef)
                (match ef
                  [(nop) (values)]
                  [(if ,[(Pred uvar*) ->] ,[] ,[]) (values)]
                  [(begin ,[] ... ,[]) (values)]
                  [(let ([,new-uvar* ,[(Value uvar*) ->]] ...) ,ef)
                   (set! all-uvar* (append new-uvar* all-uvar*))
                   ((Effect (append new-uvar* uvar*)) ef)]
                  [(,prim ,x* ...)
                   (guard (assq prim effect-prims))
                   (unless (= (length x*) (cdr (assq prim effect-prims)))
                     (error who "too many or few arguments ~s for ~s" (length x*) prim))
                   (for-each (Value uvar*) x*)
                   (values)]
                  [(,x ,y ...)
                   (guard (and (symbol? x) (not (or (uvar? x) (label? x)))))
                   (error who "invalid Effect ~s" `(,x ,y ...))]
                  [(,[(Value uvar*) ->] ,[(Value uvar*) ->] ...) (values)]
                  [,ef (error who "invalid Effect ~s" ef)]))))
          (define Pred
            (lambda (uvar*)
              (lambda (pr)
                (match pr
                  [(true) (values)]
                  [(false) (values)]
                  [(if ,[] ,[] ,[]) (values)]
                  [(begin ,[(Effect uvar*) ->] ... ,[]) (values)]
                  [(let ([,new-uvar* ,[(Value uvar*) ->]] ...) ,pr)
                   (set! all-uvar* (append new-uvar* all-uvar*))
                   ((Pred (append new-uvar* uvar*)) pr)]
                  [(,prim ,x* ...)
                   (guard (assq prim pred-prims))
                   (unless (= (length x*) (cdr (assq prim pred-prims)))
                     (error who "too many or few arguments ~s for ~s" (length x*) prim))
                   (for-each (Value uvar*) x*)
                   (values)]
                  [,pr (error who "invalid Pred ~s" pr)]))))
            (lambda (x) ((Value fml*) x))))
    (define Lambda
      (lambda (label*)
        (lambda (x)
          (match x
            [(lambda (,fml* ...) ,[(Body label* fml*) ->])
             (set! all-uvar* (append fml* all-uvar*))
             (values)]
            [,x (error who "invalid Lambda ~a" x)]))))
    (match x
      [(letrec ([,label* ,[(Lambda label*) ->]] ...) ,[(Body label* '()) ->])
       (verify-x-list label* label? 'label)
       (verify-x-list all-uvar* uvar? 'uvar)]
      [,x (error who "invalid Program ~s" x)])))
  (lambda (x) (Program x) x))


;;--------------------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------------------

;;; Grammar Remains the same from previous pass
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
				[,x (guard (or (label? x) (uvar? x))) x]
				[(quote ,[Immediate -> imm]) imm]
				[(if ,[Pred -> test] ,[Value -> conseq] ,[Value -> alt]) `(if ,test ,conseq ,alt)]
				[(begin ,[Effect -> ef*] ... ,[Value -> ef]) `(begin ,ef* ... ,ef)]
				[(let ([,uvar* ,[Value -> value*]] ...) ,[Value -> tail]) 
					`(let ([,uvar* ,value*] ...) ,tail)]
				[(void) $void] ;;In case void is encountered return its value
				;;; Multiplication I have handled using a different case as we dont need to shift both the operands by 8 everytime
				[(* ,[Value -> a] ,[Value -> b])
					(cond
						[(and (integer? a) (integer? b)) `(* ,(sra a 3) ,b)]
						[(integer? a) `(* ,b ,(sra a 3))]
						[(integer? b) `(* ,a ,(sra b 3))]
						[else `(* ,a (sra ,b 3))])]
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