;;; Shiv Indap
;;; sindap
;;; Assignment-11
;;; P523

;;; Andy Keep, Kent Dybvig
;;; P423/P523
;;; Spring 2009

;;; verify-scheme accept a single value and verifies that the value
;;; is a valid program in the current source language.
;;;
;;; Grammar for verify-scheme (assignment 11):
;;;
;;;  Program --> <Expr>
;;;  Expr   --> <label>
;;;          |  <uvar>
;;;          |  (quote <Immediate>)
;;;          |  (if <Expr> <Expr> <Expr>)
;;;          |  (begin <Expr>* <Expr>)
;;;          |  (let ([<uvar> <Expr>]*) <Expr>)
;;;          |  (letrec ([<label> (lambda (<uvar>*) <Expr>)]*) <Expr>)
;;;          |  (<primitive> <Expr>*)
;;;          |  (<Expr> <Expr>*)
;;;  Immediate -> <fixnum> | () | #t | #f
;;;
;;; Where uvar is symbol.n, n >= 0
;;;       label is symbol$n, n >= 0
;;;       fixnum is an exact integer
;;;       primitives are void (zero arguments); car, cdr, vector-length,
;;;         make-vector, boolean?, fixnum?, null?, pair?, vector? (one
;;;         argument); *, +, -, cons, vector-ref, <, <=, =, >=, >, eq?,
;;;         set-car!, set-cdr!  (two arguments); and vector-set! (three
;;;         arguments).
;;;
;;; Within the same Program, each label bound by a letrec expression
;;; must have a unique suffix, and each uvar bound by a lambda or let
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
      (null? . 1) (pair? . 1) (set-car! . 2) (set-cdr! . 2)
      (vector? . 1) (vector-length . 1) (vector-ref . 2)
      (vector-set! . 3) (void . 0)))
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
      (define all-label* '())
      (define all-uvar* '())
      (define Expr
        (lambda (label* uvar*)
          (lambda (x)
            (match x
              [,label (guard (label? label))
               (if (memq label label*)
                   (values)
                   (error who "unbound label ~s" label))]
              [,uvar (guard (uvar? uvar))
               (if (memq uvar uvar*)
                   (values)
                   (error who "unbound uvar ~s" uvar))]
              [(quote ,[Immediate ->]) (values)]
              [(if ,[] ,[] ,[]) (values)]
              [(begin ,[] ... ,[]) (values)]
              [(let ([,new-uvar* ,[]] ...) ,x)
               (set! all-uvar* (append new-uvar* all-uvar*))
               ((Expr label* (append new-uvar* uvar*)) x)]
              [(letrec ([,new-label* (lambda (,fml** ...) ,x*)] ...) ,x)
               (set! all-label* (append new-label* all-label*))
               (let ([label* (append new-label* label*)])
                 (for-each
                   (lambda (fml* x)
                     (set! all-uvar* (append fml* all-uvar*))
                    ; pass along fml* only---no free variables yet!
                     ((Expr label* fml*) x))
                   fml**
                   x*)
                 ((Expr label* uvar*) x))]
              [(,prim ,x* ...)
               (guard (assq prim primitives))
               (unless (= (length x*) (cdr (assq prim primitives)))
                 (error who "too many or few arguments ~s for ~s" (length x*) prim))
               (for-each (Expr label* uvar*) x*)
               (values)]
              [(,x ,y ...)
               (guard (and (symbol? x) (not (or (uvar? x) (label? x)))))
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
      ((Expr '() '()) x)
      (verify-x-list all-label* label? 'label)
      (verify-x-list all-uvar* uvar? 'uvar)))
  (lambda (x) (Program x) x))


;;----------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------

;;; Grammar for lift-letrec (assignment 11):
;;;
;;;  Program --> <Expr>
;;;  Expr   --> <label>
;;;          |  <uvar>
;;;          |  (quote <Immediate>)
;;;          |  (if <Expr> <Expr> <Expr>)
;;;          |  (begin <Expr>* <Expr>)
;;;          |  (let ([<uvar> <Expr>]*) <Expr>)
;;;          |  (letrec ([<label> (lambda (<uvar>*) <Expr>)]*) <Expr>)
;;;          |  (<primitive> <Expr>*)
;;;          |  (<Expr> <Expr>*)
;;;  Immediate -> <fixnum> | () | #t | #f
;;;
;;; Where uvar is symbol.n, n >= 0
;;;       label is symbol$n, n >= 0
;;;       fixnum is an exact integer
;;;       primitives are void (zero arguments); car, cdr, vector-length,
;;;         make-vector, boolean?, fixnum?, null?, pair?, vector? (one
;;;         argument); *, +, -, cons, vector-ref, <, <=, =, >=, >, eq?,
;;;         set-car!, set-cdr!  (two arguments); and vector-set! (three
;;;         arguments).


(define-who lift-letrec
	;; Defines all the primitives in the Scheme Language
	(define primitives
    '(+ - * <= < = >= > boolean? car cdr cons eq? fixnum? make-vector null? pair? set-car! set-cdr! vector? vector-length vector-ref vector-set! void ))
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
			


;;----------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------

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

;;; Ideally the input grammar should be in this form, however we may ecounter syntactic forms that may not adhere to this
;;; particular grammar, but yet we have to convert it to this form

(define-who normalize-context
	;;; The next 3 functions demarcate the various operators in the context in which they maybe encountered
	(define val-primitive?
		(lambda (x)
			(memq x '(+ * - car cdr cons make-vector vector-length vector-ref ))))
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
					`(if (eq? (,value-prim ,val* ...) '#f) (false) (true))])))
	(lambda (prog)
		(match prog
			[(letrec ([,label* (lambda (,uvar* ...) ,[Value -> val*])] ...) ,[Value -> tail]) 
				`(letrec ([,label* (lambda (,uvar* ...) ,val*)] ...) ,tail)]
			[,x (error who "invalid program ~s" x)])))
			


;;----------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------

;;;Program	----->	(letrec ([label (lambda () Tail)]*) Tail)
;;;Tail ----->		(Triv)
;;;				|		  (if (relop Triv Triv) (,label) (,label))
;;;				|	    (begin Effect* Tail)
;;;Effect	----->	(set! Loc Triv)
;;;				|				(set! Loc (binop Triv Triv))
;;;Loc		----->   reg | disp-opnd
;;;Triv	-----> Loc | int | label


(define optimize-jumps
	(lambda (prog)
	(define assoc-list '())
	;;builds up the association list and also removes lambdas containing only a jump
	(define build-assoc-list
		(lambda (label* body* last*)
			(remq '() (map (lambda (label body last)
							(if (and (null? body) (label? last))
								 (let [(symbol (walk last assoc-list '#f))]
									(cond
										[(eq? symbol '#f) (begin (set! assoc-list (cons (cons label last) assoc-list)) '())] ;when the pairs are completely independent
										[(eq? symbol label) (begin (set! assoc-list (append assoc-list '())) `(,label (lambda () (,@body ,last))))];; when there is a cycle e.g (3 . 2) ((1 . 2) (2 . 3))
										;; will result in ((1 . 2) (2 . 3))
										[else (begin (set! assoc-list (cons (cons label symbol) assoc-list)) '())])) ;;all other cases										
								`(,label (lambda () (,@body ,last))))) label* body* last*))))
	(define resolve-label
		(lambda (assoc-list label)
			(let ([value (walk label assoc-list '#f)])
				(if (eq? value '#f) label value))))
	;;; walks through the associated list and gets the value of the label associated with val
	;;; e.g walk '1 '((1 . 2) (2 . 3)) will give the value 3
	;;; if not found it retuns #f
	(define walk
		(lambda (val ls sym)
			(cond
				[(null? ls) sym]
				[(assq val ls) (let ([new-val (cdr (assq val ls))])
													(walk new-val assoc-list new-val))]
				[else (walk val (cdr ls) sym)])))
	(define Driver
		(lambda (x)
			(set! assoc-list '()) ;;;List updated using side effects
			(match x
				[(letrec ([,label* (lambda () (,tail* ... ,last*))] ...) ,tail)
					(let ([funcs* (build-assoc-list label* tail* last*)])
						(Prog `(letrec (,funcs* ...) ,tail) assoc-list))])))
	;;; Once the association list has been built, process the incoming code passing the association list along
	(define Prog
		(lambda (pro assoc-list)
			(match pro
				[(letrec ([,label* (lambda () ,[(Tail assoc-list) -> tail*])] ... ),[(Tail assoc-list) -> tail])
					`(letrec ([,label* (lambda () ,tail*)] ...) ,tail)])))
	(define Effect
		(lambda (assoc-list)
			(lambda (effect)
				(match effect
					[(set! ,x ,[(Triv assoc-list) -> y]) `(set! ,x ,y)]
					[(set! ,x (,binop ,[(Triv assoc-list) -> y] ,[(Triv assoc-list) -> z])) `(set! ,x (,binop ,y ,z))]))))
	(define Triv
		(lambda (assoc-list)
			(lambda (triv)
				(match triv
					[,x (guard (label? x)) (resolve-label assoc-list x)] ;; If a label is found replace the label by its corresponding value
					[,y y]))))
	(define Tail
		(lambda (assoc-list)
			(lambda (tail)
				(match tail
					[(if (,relop ,[(Triv assoc-list) -> a] ,[(Triv assoc-list) -> b]) (,conseq-label) (,alt-label))
						(let ([new-conseq-label (resolve-label assoc-list conseq-label)]
									[new-alt-label (resolve-label assoc-list alt-label)])
									`(if (,relop ,a ,b) (,new-conseq-label) (,new-alt-label)))]
					[(begin ,[(Effect assoc-list) -> ef*] ... ,[(Tail assoc-list) -> tail])
						`(begin ,ef* ... ,tail)]
					[(,[(Triv assoc-list) -> triv]) `(,triv)]))))	
				(Driver prog)))

;;;Output Grammar of the Pass is same as input grammar



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
