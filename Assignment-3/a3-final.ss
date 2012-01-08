(load "match.ss")
(load "helpers.ss")

;;; Grammar assignment 3:
;;;
;;; Program --> (letrec ([<label> (lambda () <Body>)]*) <Body>)
;;; Body    --> (locate ([uvar <Loc>]*) <Tail>)
;;; Tail    --> (<Triv>)
;;;          |  (begin <Effect>* <Tail>)
;;;          |  (if <Pred> <Tail> <Tail>)
;;; Pred    --> (true)
;;;          |  (false)
;;;          |  (predop <Triv> <Triv>)
;;;          |  (begin <Effect>* <Pred>)
;;;          |  (if <Pred> <Pred> <Pred>)
;;; Effect  --> (nop)
;;;          |  (set! <Var> <Triv>)
;;;          |  (set! <Var> (<binop> <Triv> <Triv>)
;;;          |  (begin <Effect>+)
;;;          |  (if <Pred> <Effect> <Effect>)
;;; Var     --> uvar
;;;          |  Loc
;;; Loc     --> register
;;;          |  frame-var
;;; Triv    --> Var
;;;          |  int
;;;          |  label
;;;
;;; Where uvar is symbol.n where (n >= 0)
;;;       binop is +, -, *, logand, logor, or sra
;;;       predop is <, <=, =, >=, or >
;;;       register is rax, rcx, rdx, rbx, rbp, rdi, rsi, r8,
;;;                   r9, r10, r11, r12, r13, r14, or r15
;;;       label is symbol$n where (n >= 0)
;;;       frame-var is fvn where (n >= 0)
;;;
;;; If the value is a valid program, verify scheme returns the value
;;; unchanged; otherwise, it signals an error.

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
  (define Var
    (lambda (env)
      (lambda (var)
        (unless (or (register? var) (frame-var? var) (uvar? var))
          (error who "invalid variable ~s" var))
        (when (uvar? var)
          (unless (assq var env)
            (error who "unbound uvar ~s" var)))
        var)))
  (define Loc
    (lambda (loc)
      (unless (or (register? loc) (frame-var? loc))
        (error who "invalid Loc ~s" loc))
      loc))
  (define Var->Loc
    (lambda (v env)
      (if (uvar? v) (cdr (assq v env)) v)))
  (define Triv
    (lambda (label* env)
      (lambda (t)
        (unless (or (register? t) (frame-var? t) (label? t) (uvar? t)
                    (and (integer? t) (exact? t)))
          (error who "invalid Triv ~s" t))
        (when (uvar? t)
          (unless (assq t env)
            (error who "unbound uvar ~s" t)))
        (when (label? t)
          (unless (memq t label*)
            (error who "unbound label ~s" t)))
        t)))
  (define Pred
    (lambda (label* env)
      (lambda (pr)
        (match pr
          [(true) (void)]
          [(false) (void)]
          [(begin ,[(Effect label* env) -> ef*] ... ,[pr]) (void)]
          [(if ,[test] ,[conseq] ,[altern]) (void)]
          [(,predop ,[(Triv label* env) -> x] ,[(Triv label* env) -> y])
           (unless (memq predop '(= < <= > >=))
             (error who "invalid predicate operator ~s" predop))
           (let ([x (Var->Loc x env)] [y (Var->Loc y env)])
             (unless (or (and (register? x)
                              (or (register? y)
                                  (frame-var? y)
                                  (int32? y)))
                         (and (frame-var? x)
                              (or (register? y)
                                  (int32? y))))
               (error who "~s violates machine constraints"
                      `(,predop ,x ,y))))]
          [,pr (error who "invalid Pred ~s" pr)]))))
  (define Effect
    (lambda (label* env)
      (lambda (ef)
        (match ef
          [(nop) (void)]
          [(set! ,[(Var env) -> x]
             (,binop ,[(Triv label* env) -> y] ,[(Triv label* env) -> z]))
           (unless (and (eq? y x)
                        (let ([x (Var->Loc x env)] [z (Var->Loc z env)])
                          (case binop
                            [(+ - logand logor)
                             (or (and (register? x)
                                      (or (register? z)
                                          (frame-var? z)
                                          (int32? z)))
                                 (and (frame-var? x)
                                      (or (register? z)
                                          (int32? z))))]
                            [(*)
                             (and (register? x)
                                  (or (register? z)
                                      (frame-var? z)
                                      (int32? z)))]
                            [(sra)
                             (and (or (register? x) (frame-var? x))
                                  (uint6? z))]
                            [else
                             (error who "invalid binary operator ~s" binop)])))
             (error who "~s violates machine constraints"
                    `(set! ,x (,binop ,y ,z))))]
          [(set! ,[(Var env) -> x] ,[(Triv label* env) -> y])
           (let ([x (Var->Loc x env)] [y (Var->Loc y env)])
             (unless (or (and (register? x)
                              (or (register? y)
                                  (frame-var? y)
                                  (int64? y)
                                  (label? y)))
                         (and (frame-var? x)
                              (or (register? y)
                                  (int32? y))))
               (error who "~s violates machine constraints" `(set! ,x ,y))))]
          [(begin ,[ef] ,[ef*] ...) (void)]
          [(if ,[(Pred label* env) -> test] ,[conseq] ,[altern]) (void)]
          [,ef (error who "invalid Effect ~s" ef)]))))
  (define Tail
    (lambda (label* env)
      (lambda (tail)
        (match tail
          [(begin ,[(Effect label* env) -> ef*] ... ,tail)
           ((Tail label* env) tail)]
          [(if ,[(Pred label* env) -> test] ,[conseq] ,[altern]) (void)]
          [(,[(Triv label* env) -> t])
           (when (integer? t)
             (error who "~s violates machine constraints" `(,t)))]
          [,tail (error who "invalid Tail ~s" tail)]))))
  (define Body
    (lambda (label*)
      (lambda (bd)
        (match bd
          [(locate ([,uvar* ,[Loc -> loc*]] ...) ,tail)
           (verify-x-list uvar* uvar? 'uvar)
           ((Tail label* (map cons uvar* loc*)) tail)]
          [,bd (error who "invalid Body ~s" bd)]))))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,bd*)] ...) ,bd)
       (verify-x-list label* label? 'label)
       (for-each (Body label*) bd*)
       ((Body label*) bd)]
      [,x (error who "invalid Program ~s" x)])
    x))


; Finalize locations Pass
;------------------------------------------------------------------------------------------------------------------------------------------------------
(define finalize-locations
	(lambda (prog)
		(define build-funcs
			(lambda (label* body*)
				(map build-function label* body*)))
		(define build-function
			(lambda (label body)
				`(,label (lambda () ,(Body body)))))
		(define build-env
			(lambda (uvar* loc*)
				(map (lambda (x y) (cons x y)) uvar* loc*)))
		(define replace-uvar
			(lambda (effect* env)
				(map (lambda (proc) (proc env)) (map Effect effect*))))
		(define Program
			(lambda (x)
				(match x
					[(letrec ([,label* (lambda () ,body*)] ...) ,body) 
						(let ((letrec-body (Body body)) (funcs (build-funcs label* body*)))
							`(letrec ,funcs ,letrec-body))])))
		(define Body
			(lambda (x)
				(match x
					[(locate ([,uvar* ,loc*] ...) ,tail) 
						(let ((env (build-env uvar* loc*)))
							(Tail tail env))])))
		(define Effect
			(lambda (effect)
				(lambda (env)
					(match effect
						[(begin ,effect* ... ,effect)
						 	(let ((effects (replace-uvar effect* env)) (parsed-tail ((Effect effect) env)))
								`(begin ,@effects ,parsed-tail))]
						[(set! ,var (,binop ,triv1 ,triv2)) `(set! ,(Var var env) (,binop ,(Triv triv1 env) ,(Triv triv2 env)))]
						[(set! ,var ,triv) `(set! ,(Var var env) ,(Triv triv env))]
						[(nop) '(nop)]

						[(if ,pred ,conseq ,alt) `(if ,(Pred pred env) ,((Effect conseq) env) ,((Effect alt) env))]))))
		(define Pred
			(lambda (x env)
				(match x
					[(true) (list 'true)]
					[(false) (list 'false)]
					[(,relop ,triv1 ,triv2) `(,relop ,(Triv triv1 env) ,(Triv triv2 env))]
					[(if ,conde ,conseq ,alt) `(if ,(Pred conde env) ,(Pred conseq env) ,(Pred alt env))])))
		(define Triv
			(lambda (x env)
				(if (or (integer? x) (label? x)) x (Var x env))))
		(define Var
			(lambda (x env)
				(if (uvar? x) (cdr (assq x env)) (Loc x))))
		(define Loc
			(lambda (x)
				x))
		(define Tail
			(lambda (x env)
				(match x
					[(begin ,effect* ... ,tail)
						 (let ((effects (replace-uvar effect* env)) (parsed-tail (Tail tail env)))
							`(begin ,@effects ,parsed-tail))]
					[(if ,pred ,tail1 ,tail2) `(if ,(Pred pred env) ,(Tail tail1 env) ,(Tail tail2 env))]
					[(,triv) (list (Triv triv env))])))
	(Program prog)))

;expose-frame-var pass
;------------------------------------------------------------------------------------------------------------------------------------------------------
	(define expose-frame-var
		(lambda (prog)
			(define build-funcs
				(lambda (label* body*)
					(map build-function label* body*)))
			(define build-function
				(lambda (label body)
					`(,label (lambda () ,(Tail body)))))
		(define Program
			(lambda (x)
				(match x
					[(letrec ([,label* (lambda () ,tail*)] ...) ,tail) 
						(let ((letrec-tail (Tail tail)) (funcs (build-funcs label* tail*)))
							`(letrec ,funcs ,letrec-tail))])))
		(define Effect
			(lambda (effect)
				(match effect
					[(begin ,effect* ... ,effect)
						(let ((effect*-parsed (map Effect effect*)) (effect-parsed (Effect effect)))
							`(begin ,@effect*-parsed ,effect-parsed))]
					[(set! ,loc (,binop ,triv1 ,triv2)) `(set! ,(Loc loc) (,binop ,(Triv triv1) ,(Triv triv2)))]
					[(set! ,loc ,triv) `(set! ,(Loc loc) ,(Triv triv))]
				  [(nop) '(nop)]
					[(if ,pred ,conseq ,alt) `(if ,(Pred pred) ,(Effect conseq) ,(Effect alt))])))
		(define Pred
			(lambda (x)
				(match x
					[(true) (list 'true)]
					[(false) (list 'false)]
					[(,relop ,triv1 ,triv2) `(,relop ,(Triv triv1) ,(Triv triv2))]
					[(if ,conde ,conseq ,alt) `(if ,(Pred conde) ,(Pred conseq) ,(Pred alt))]
					[(begin ,effect* ..., body)
						(let ((effect*-parsed (map Effect effect*)) (pred-parsed (Pred body)))
							`(begin ,@effect*-parsed ,pred-parsed))])))
		(define Triv
			(lambda (x)
				(if (or (integer? x) (label? x)) x (Loc x))))
		(define Loc
			(lambda (x)
				(if (frame-var? x) (make-disp-opnd 'rbp (ash (frame-var->index x) 3))
				x)))
		(define Tail
			(lambda (x)
				(match x
					[(begin ,effect* ... ,tail) 
						(let ((effect*-parsed (map Effect effect*)) (tail-parsed (Tail tail)))
							`(begin ,@effect*-parsed ,tail-parsed))]
					[(if ,pred ,tail1 ,tail2) `(if ,(Pred pred) ,(Tail tail1) ,(Tail tail2))]
					[(,triv) (list (Triv triv))])))
		(Program prog)))
		
;;----------Begin expose-basic Blocks---------------------------
(define expose-basic-blocks
	(lambda (prog)
		(define Program
			(lambda (x)
				(match x
					[(letrec ([,label* (lambda () ,[Tail -> bindings* tail*])] ...) ,[Tail -> binding tail])
							`(letrec ,(fold-right (lambda(x y z ls) (append (append (bind-function x y) z) ls)) binding label* tail* bindings*) ,tail)
							])))
		(define Tail
			(lambda (x)
				(match x
					[(if ,condition ,[Tail -> cbinding conseq] ,[Tail -> abinding alt]) 
						(let ((conseq-label (unique-label 'c))
									(alt-label (unique-label 'a)))
									(let-values ([(bindings statements) (Pred condition conseq-label alt-label)]) 
										(values (append cbinding abinding bindings (bind-function conseq-label conseq) (bind-function alt-label alt)) statements)))]
					[(begin ,effect* ...,[Tail -> binding tail])
					 		(let-values ([(bindings statements) (Effect* effect* (list tail))])
								(values (append binding bindings) statements))]		
					[(,triv) (values '() `(,triv))])))
		(define Effect*
			(lambda (ef* code)
				(match ef*
					[() (values '() (if (null? code) '() (make-begin (if (list? (car code)) code `(,code)))))]
					[(,effect* ...,effect) (Effect effect* effect code)]))) 
		(define Effect
			(lambda (before* expr after*)
				(match expr
					[(if ,condition ,conseq ,alt)
						(let ((conseq-label (unique-label 'c))
									(alt-label (unique-label 'a))
									(jmp-label (unique-label 'j)))
							(let*-values
								([(p-bindings p-statements) (Pred condition conseq-label alt-label)]
								 [(c-bindings c-statements) (Effect '() conseq (list (list jmp-label)))]
								 [(a-bindings a-statements) (Effect '() alt (list (list jmp-label)))]
								 [(e-bindings e-statements) (Effect* before* (list p-statements))])
								 (values (append (bind-function conseq-label c-statements) 
												 (bind-function alt-label a-statements)
												 (bind-function jmp-label (if (null? after*) '() (make-begin after*))) p-bindings c-bindings a-bindings e-bindings) e-statements)))]
					
					[(set! ,loc ,triv) (Effect* before* (append (list expr) (if (list? (car after*)) after* `(,after*))))]
					[(begin ,effect* ...) 
						(let*-values 
							([(e-bindings e-statements) (Effect* effect* after*)]
							 [(b-bindings statements) (Effect* before* e-statements)])
							(values (append e-bindings b-bindings) statements))]
				  [(nop) (Effect* before* after*)])))
		(define Pred
				(lambda (exp true-label false-label)
					(match exp
						[(true) (values '() `(,true-label))]
						[(false) (values '() `(,false-label))]
						[(begin ,effect* ..., body) 
							(let*-values
								([(p-bindings p-statements) (Pred body true-label false-label)]
								 [(e-bindings e-statements) (Effect* effect* p-statements)])
								(values (append e-bindings p-bindings) e-statements))]
						[(,relop ,triv1 ,triv2) (values '() `(if ,exp (,true-label) (,false-label)))]
						[(if ,conde ,conseq ,alt) 
							(let ((conseq-label (unique-label 'c))
										(alt-label (unique-label 'a)))
								(let*-values
									([(p-bindings p-statements) (Pred conde conseq-label alt-label)]
									 [(c-bindings c-statements) (Pred conseq true-label false-label)]
									 [(a-bindings a-statements) (Pred alt true-label false-label)])
									 (values (append (bind-function conseq-label c-statements) 
													 (bind-function alt-label a-statements) p-bindings c-bindings a-bindings) p-statements)))])))
		(define bind-function
			(lambda (label statement)
				`((,label (lambda () ,statement)))))						
		(Program prog)))

;;---------Begin Flatten program



;;------------
(define test
	(lambda ()
		;(flatten-program
			(expose-basic-blocks
				(expose-frame-var
					(finalize-locations
						(verify-scheme
							'(letrec ([if-test$5 (lambda ()
					                          (locate ([n.1 rdi] [x.2 rax] [y.3 rbx])
					                            (begin
					                              (set! x.2 1)
					                              (set! y.3 1)
					                              (if (= n.1 0)
					                                  (set! x.2 (+ x.2 y.3))
					                                  (set! y.3 (+ y.3 x.2)))
					                              (set! x.2 n.1)
					                              (if (if (= n.1 y.3) (false) (true))
					                                  (set! n.1 (+ n.1 x.2))
					                                  (set! n.1 (+ n.1 y.3)))
					                              (set! x.2 n.1)
					                              (r15))))])
					      (locate ([n.1 rdi])
					        (begin
					          (set! n.1 1)
					          (if-test$5))))))))));)


