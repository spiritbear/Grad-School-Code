;;; Andy Keep, Kent Dybvig
;;; P423/P523
;;; Spring 2009

;;; verify-scheme accept a single value and verifies that the value
;;; is a valid program in the grammar of the fourth assignment
;;;
;;; Grammar for verify-scheme (assignment 4):
;;;
;;; Program --> (letrec ((<label> (lambda () <Body>))*) <Body>)
;;; Body    --> (locals (<uvar>*) (locate ((<uvar> <frame-var>)*) <Tail>))
;;; Tail    --> (<Triv> <Var>*)
;;;          |  (begin <Effect>* <Tail>)
;;;          |  (if <Pred> <Tail> <Tail>)
;;; Pred    --> (true)
;;;          |  (false)
;;;          |  (<predop> <Triv> <Triv>)
;;;          |  (begin <Effect*> <Pred>)
;;;          |  (if <Pred> <Pred> <Pred>)
;;; Effect  --> (nop)
;;;          |  (set! <Var> <Triv>)
;;;          |  (set! <Var> (<binop> <Triv> <Triv>))
;;;          |  (begin <Effect>+)
;;; Var     --> <uvar>
;;;          |  <frame-var>
;;;          |  <register>
;;; Triv    --> <Var>
;;;          |  <int>
;;;          |  <label>
;;;
;;; Where uvar is symbol.n where (n >= 0)
;;;       binop is +, -, *, logand, logor, or sra
;;;       predop is <, <=, or =
;;;       register is rax, rcx, rdx, rbx, rbp, rdi, rsi, r8,
;;;                   r9, r10, r11, r12, r13, r14, or r15
;;;       label is symbol$n where (n >= 0)
;;;       frame-var is fvn where (n >= 0)
;;;
;;; If the value is a valid program, verify scheme returns the value
;;; unchanged; otherwise it signals an error.
;;;
;;; At this level in the compiler verify-scheme is also responsible for
;;; ensuring that machine constraints are not violated in generated
;;; assembler code to the best of its ability.

(case-sensitive #t)
(load "match.ss")
(load "helpers.ss")

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
  (define FrameVar
    (lambda (x)
      (unless (frame-var? x)
        (error who "invalid frame-var ~s" x))
      x))
  (define Var
    (lambda (uvar*)
      (lambda (var)
        (unless (or (register? var) (frame-var? var) (uvar? var))
          (error who "invalid variable ~s" var))
        (when (uvar? var)
          (unless (memq var uvar*)
            (error who "unbound uvar ~s" var)))
        var)))
  (define Triv
    (lambda (label* uvar*)
      (lambda (t)
        (unless (or (register? t) (frame-var? t) (label? t) (uvar? t)
                    (and (integer? t) (exact? t)))
          (error who "invalid Triv ~s" t))
        (when (uvar? t)
          (unless (memq t uvar*)
            (error who "unbound uvar ~s" t)))
        (when (label? t)
          (unless (memq t label*)
            (error who "unbound label ~s" t)))
        t)))
  (define Pred
    (lambda (label* uvar*)
      (lambda (pr)
        (match pr
          [(true) (void)]
          [(false) (void)]
          [(begin ,[(Effect label* uvar*) -> ef*] ... ,[pr]) (void)]
          [(if ,[test] ,[conseq] ,[altern]) (void)]
          [(,predop ,[(Triv label* uvar*) -> x]
                    ,[(Triv label* uvar*) -> y])
           (unless (memq predop '(= < <= > >=))
             (error who "invalid predicate operator ~s" predop))
           (unless (or (and (or (register? x) (memq x uvar*))
                            (or (register? y)
                                (memq y uvar*)
                                (frame-var? y)
                                (int32? y)))
                       (and (frame-var? x)
                            (or (register? y)
                                (memq y uvar*)
                                (int32? y))))
             (error who "~s violates machine constraints"
                    `(,predop ,x ,y)))]
          [,pr (error who "invalid Pred ~s" pr)]))))
  (define Effect
    (lambda (label* uvar*)
      (lambda (ef)
        (match ef
          [(nop) (void)]
          [(set! ,[(Var uvar*) -> x]
             (,binop ,[(Triv label* uvar*) -> y]
                     ,[(Triv label* uvar*) -> z]))
           (unless (and (eq? y x)
                        (case binop
                          [(+ - logand logor)
                           (or (and (or (register? x) (memq x uvar*))
                                    (or (register? z)
                                        (memq z uvar*)
                                        (frame-var? z)
                                        (int32? z)))
                               (and (frame-var? x)
                                    (or (register? z)
                                        (memq z uvar*)
                                        (int32? z))))]
                          [(*)
                           (and (or (register? x) (memq x uvar*))
                                (or (register? z)
                                    (memq z uvar*)
                                    (frame-var? z)
                                    (int32? z)))]
                          [(sra)
                           (and (or (register? x) (frame-var? x) (memq x uvar*))
                                (uint6? z))]
                          [else
                            (error who "invalid binary operator ~s" binop)]))
             (error who "~s violates machine constraints"
                    `(set! ,x (,binop ,y ,z))))]
          [(set! ,[(Var uvar*) -> x] ,[(Triv label* uvar*) -> y])
           (unless (or (and (or (register? x) (memq x uvar*))
                            (or (register? y)
                                (memq y uvar*)
                                (frame-var? y)
                                (int64? y)
                                (label? y)))
                       (and (frame-var? x)
                            (or (register? y)
                                (memq y uvar*)
                                (int32? y))))
               (error who "~s violates machine constraints" `(set! ,x ,y)))]
          [(begin ,[ef] ,[ef*] ...) (void)]
          [(if ,[(Pred label* uvar*) -> test] ,[conseq] ,[altern]) (void)]
          [,ef (error who "invalid Effect ~s" ef)]))))
  (define Tail
    (lambda (label* uvar*)
      (lambda (tail)
        (match tail
          [(begin ,[(Effect label* uvar*) -> ef*] ... ,[tail]) (void)]
          [(if ,[(Pred label* uvar*) -> test] ,[conseq] ,[altern]) (void)]
          [(,[(Triv label* uvar*) -> t] ,[(Var uvar*)-> live-out*] ...)
           (when (integer? t)
             (error who "~s violates machine constraints" `(,t)))]
          [,tail (error who "invalid Tail ~s" tail)]))))
  (define Body
    (lambda (label*)
      (lambda (bd)
        (match bd
          [(locals (,uvar* ...) ,tail)
           (verify-x-list `(,uvar* ...) uvar? 'uvar)
           ((Tail label* uvar*) tail)]
          [,bd (error who "invalid Body ~s" bd)]))))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,bd*)] ...) ,bd)
       (verify-x-list label* label? 'label)
       (for-each (Body label*) bd*)
       ((Body label*) bd)]
      [,x (error who "invalid Program ~s" x)])
    x))



(define-who uncover-register-conflict
	(define update-conflict-table
		(lambda (var live* ct)
			(let ((conflict-entry (assq var ct)))
				(set-cdr! conflict-entry (union live* (if (null? (cdr conflict-entry)) '() (cdr conflict-entry))))
				(map (lambda (x)
								(if (uvar? x)
											(let ((conflict-entry (assq x conflict-table)))
																(set-cdr! conflict-entry (union (list var) (if (null? (cdr conflict-entry)) '() (cdr conflict-entry))))))
								x) live*))))
  (define Triv
    (trace-lambda Triv(x)
      (if (or (register? x) (uvar? x)) x '())))
  (define Effect*
    (trace-lambda Effect*(x live* ct)
      (match x
        [() live*]
        [(,ef* ... ,ef) (Effect* ef* (Effect ef live* ct) ct)]
        [,x (error who "invalid Effect* list ~s" x)])))
  (define Effect
    (trace-lambda Effect(x live* ct)
      (match x
        [(nop) live*]
        [(if ,test ,[c-live*] ,[a-live*])
         (Pred test c-live* a-live* ct)]
        [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
        [(set! ,lhs (,binop ,[Triv -> x-live*] ,[Triv -> y-live*])) (resolve-lhs-double lhs x-live* y-live* live* ct)]
        [(set! ,lhs ,[Triv -> var]) (resolve-lhs-single lhs var live* ct)]
        [,x (error who "invalid Effect list ~s" x)])))
  (define resolve-lhs-double 
		(lambda (lhs x* y* live* ct)
			(cond
				[(register? lhs) (if (member lhs live*) (func-2 lhs x* y* live*) live*)]
				[(uvar? lhs)
						(let ((new-live (func-2 lhs x* y* live*)))
							(modify-conflict-table lhs new-live ct))]
				[else live*])))
	(define modify-conflict-table
		(lambda (lhs new-live ct)
			(let ((ct-entry (cdr (assv lhs ct))))
				(if (null? ct-entry)
					(set-cdr! (assq lhs ct) (difference new-live (list lhs)))
					(set-cdr! (assq lhs ct) (difference (union new-live ct-entry) (list lhs))))
					new-live)))
	(define resolve-lhs-single
		(trace-lambda single(lhs rhs live* ct)
			(cond 
				[(register? lhs) (if (member lhs live*) (func-1 lhs rhs live*) (if (null? rhs) live* (set-cons rhs live*)))]
				[(uvar? lhs)
					(let ((new-live (func-1 lhs rhs live*)) (ct-entry (cdr (assv lhs ct))))
						(if (null? ct-entry) 
							(set-cdr! (assq lhs ct) (difference new-live (list lhs)))
							(set-cdr! (assq lhs ct) (difference (union new-live ct-entry) (list lhs))))
							new-live)]
				[else (set-cons rhs live*)])))
	(define func-1
		(lambda (lhs rhs live*)
			(if (null? rhs) (difference live* (list lhs)) (set-cons rhs (difference live* (list lhs))))))
	(define func-2
		(lambda (lhs x y live*)
			(cond
				[(null? x) (set-cons y (difference live* (list lhs)))]
				[(null? y) (set-cons x (difference live* (list lhs)))]
				[else (set-cons x (set-cons y (difference live* (list lhs))))])))					
  (define Pred
    (trace-lambda Pred(x t-live* f-live* ct)
      (match x
        [(true) t-live*]
        [(false) f-live*]
        [(if ,test ,[c-live*] ,[a-live*])
         (union c-live* a-live*)]
        [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
        [(,predop ,[Triv -> x-live*] ,[Triv -> y-live*])
         (remove-nulls (union (list x-live*) (list y-live*) t-live* f-live*))]
        [,x (error who "invalid Pred ~s" x)])))
  (define Tail
    (trace-lambda Tail(x ct)
      (match x
        [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
        [(if ,test ,[c-live*] ,[a-live*])
         (union (list c-live*) (list a-live*))]
        [(,target ,live* ...)
         (remove-nulls (cons (Triv target) (map Triv live*)))]
        [,x (error who "invalid Tail ~s" x)])))
  (define remove-nulls
		(lambda (ls)
			(cond
				[(null? ls) '()]
				[(null? (car ls)) (remove-nulls (cdr ls))]
				[else (cons (car ls) (remove-nulls (cdr ls)))])))
  (define Body
    (trace-lambda Body(x)
      (match x
        [(locals (,uvar* ...) ,tail)
         ;; set up the conflict table ct for storing conflicts
         ;; with initial empty list of conflicts for each uvar
         (let ([ct (map (lambda (x) (cons x '())) uvar*)])
           (let ([live* (Tail tail ct)])
             `(locals (,uvar* ...)
                (register-conflict ,ct ,tail))))]
        [,x (error who "invalid Body ~s" x)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,x (error who "invalid Program ~s" x)])))

;(define-who assign-registers
;  (define k (length registers))
;  (define low-degree?
;    (lambda (var)
;      (< (length (cdr (assq var ct))) k)))
;  (define find-homes
;    (lambda (var* ct)
;      ---))
;  (define Body
;    (lambda (x)
;      (match x
;        [(locals (,uvar* ...) (register-conflict ,ct ,tail))
;         (let ([home* (find-homes uvar* ct)])
;           (let ([spill* (difference (map car home*) uvar*)])
;             (if (null? spill*)
;                 `(locate (,home* ...) ,tail)
;                 (error who "unable to assign registers to ~s" spill*))))]
;        [,x (error who "invalid Body ~s" x)])))
;  (lambda (x)
;    (match x
;      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
;       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
;      [,x (error who "invalid Program ~s" x)])))
;
;(define-who discard-call-live
;  (define Tail
;    (lambda (tail)
;      (match tail
;        ---
;        [,tail (error who "invalid Tail ~s" tail)])))
;  (define Body
;    (lambda (bd)
;      (match bd
;        ---
;        [,bd (error who "invalid Body ~s" bd)])))
;  (lambda (x)
;    (match x
;      [(letrec ([,label* (lambda () ,[Body -> bd*])] ...) ,[Body -> bd])
;       `(letrec ([,label* (lambda () ,bd*)] ...) ,bd)]
;      [,x (error who "invalid Program ~s" x)])))

(define test
	(lambda ()
		(uncover-register-conflict
			(verify-scheme 
				'(letrec ([if-test$1 (lambda ()
		                           (locals (x.5 y.2)
		                             (begin
																	 (set! x.5 5)
																	 (set! y.2 2)
		                               (if (= x.5 0)
		                                   (set! x.5 (+ x.5 12))
		                                   (set! x.5 (- x.5 10)))
		                               (set! x.5 (* x.5 10))
		                               (set! rdx x.5)
		                               (r15 rax))))])
		       (locals () (if-test$1 r15)))
				))))

(define x
	(lambda (arg)
		(if (null? arg) '() arg)))
