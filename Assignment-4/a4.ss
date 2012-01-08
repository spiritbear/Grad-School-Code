;;; Assignment-4
;;; Shiv Indap
;;; sindap
;;; P-523


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

;;------------------------------------------------------------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------------------------------------------------------------

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


;; This section of the code simply prepares a live-set i.e the set of variables live before an assignment instruction is encountered
;; The update conflict table is responsible for updating the conflict table ct using side effects
;; if x.1 is current Lhs and {a.1 r15 rax} is live set we add the live-set to x.1's conflict
;; For an if-instruction encountered we take the union of both the conseqent and alternative parts and combine it
;; Processing of this code goes in a bottom up manner

;;------------------------------------------------------------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------------------------------------------------------------

(define-who uncover-register-conflict
	(define update-conflict-table
		(lambda (var live* ct)
			(if (register? var) live*)
			(let ((conflict-entry (assq var ct)) (live-vars (remq var live*)))
				(set-cdr! conflict-entry (union live-vars (if (null? (cdr conflict-entry)) '() (cdr conflict-entry))))
				(cons var (map (lambda (x)
								(if (uvar? x)
											(let ((x-conflict-entry (assq x ct)))
																(set-cdr! x-conflict-entry (union (list var) (if (null? (cdr x-conflict-entry)) '() (cdr x-conflict-entry))))))
								x) live-vars)))))
  (define Triv
    (lambda (x)
      (if (or (register? x) (uvar? x)) x '())))
  (define Effect*
    (lambda (x live* ct)
      (match x
        [() live*]
        [(,ef* ... ,ef) (Effect* ef* (Effect ef live* ct) ct)]
        [,x (error who "invalid Effect* list ~s" x)])))
  (define Effect
    (lambda (x live* ct)
      (match x
        [(nop) live*]
        [(if ,test ,[c-live*] ,[a-live*])
         (Pred test (union live* c-live*) (union live* a-live*) ct)]
        [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
        [(set! ,lhs (,binop ,[Triv -> x-live*] ,[Triv -> y-live*])) 
						(let* ((new-live-set y-live*) (resolution (resolve lhs live* ct)))							
							(if (null? new-live-set) 
								live* (set-cons new-live-set live*)))]
        [(set! ,lhs ,[Triv -> var]) 
						(let* ((new-live-set (remq lhs live*)) (resolution (resolve lhs live* ct)))
								  (if (null? var) new-live-set (set-cons var new-live-set)))]
        [,x (error who "invalid Effect list ~s" x)])))
  (define resolve
		(lambda (lhs live* ct)
			(cond
				[(register? lhs) (record-conflict lhs live* ct)]
				[(uvar? lhs) (update-conflict-table lhs live* ct)])))
	(define record-conflict
		(lambda (lhs live* ct)
			(cond
				[(null? live*) '()]
				[(uvar? (car live*)) 
						(let* ((var (car live*)) (conflict-entry (assq var ct)))
							(set-cdr! conflict-entry (set-cons lhs (if (null? (cdr conflict-entry)) '() (cdr conflict-entry))))
							(record-conflict lhs (cdr live*) ct))]
				[else (record-conflict lhs (cdr live*) ct)])))		
	(define Pred
    (lambda (x t-live* f-live* ct)
      (match x
        [(true) t-live*]
        [(false) f-live*]
        [(if ,test ,[c-live*] ,[a-live*])
         (Pred test c-live* a-live* ct)]
        [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
        [(,predop ,[Triv -> x-live*] ,[Triv -> y-live*])
         (remove-nulls (union (list x-live*) (list y-live*) t-live* f-live*))]
        [,x (error who "invalid Pred ~s" x)])))
  (define Tail
    (lambda (x ct)
      (match x
        [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
        [(if ,test ,[c-live*] ,[a-live*])
         (union c-live* a-live*)]
        [(,target ,live* ...)
         (remove-nulls (cons (Triv target) (map Triv live*)))]
        [,x (error who "invalid Tail ~s" x)])))
  (define remove-nulls
		(lambda (ls)
			(cond
				[(null? ls) '()]
				[(null? (car ls)) (remove-nulls (cdr ls))]
				[else (set-cons (car ls) (remove-nulls (cdr ls)))])))
  (define Body
    (lambda (x)
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


;;Program	---->	(letrec ([label (lambda () Body)]*) Body)
;;Body	---->	(locals (uvar*)
;;		  (register-rconflict conflict-graph Tail))
;;Tail	---->	(Triv Loc*)
;;	|	(if Pred Tail Tail)
;;	|	(begin Effect* Tail)
;;Pred	---->	(true)
;;	|	(false)
;;	|	(relop Triv Triv)
;;	|	(if Pred Pred Pred)
;;	|	(begin Effect* Pred)
;;Effect	---->	(nop)
;;	|	(set! Var Triv)
;;	|	(set! Var (binop Triv Triv))
;;	|	(if Pred Effect Effect)
;;	|	(begin Effect* Effect)
;;Loc	---->	reg | fvar
;;Var	---->	uvar | Loc
;;Triv	---->	Var | int | label

;; This is the input grammar for the next stage which decides to assign a register to each variable whenever possible
;; The conflict graph looks something like ((a.1 r15 rax b.2) (b.2 r15 rax a.1))
;; In assign registers the crux of the functionality is in find-homes, it initially tries to find a low-degree node and proceeds recursively, if it cant it picks the node
;; with the smallest number of conflicts and proceeds recursively, each time removing the node with the least number of conflicts from the graph and stacking it
;; when a null is encountered the nodes are popped off from the stack one at a time and assigned a home, the update homes are now propogated one level above to the
;; conflict table and the next node is popped and so on, this process continues till all the registers get a home

			
(define-who assign-registers
	(define remove-occurence						;;Removes the occurence of a var from var* and returns the list
		(lambda (var ct)
			(map (lambda (x) 
							(cond
								[(eq? (car x) var) x]
								[else (remq var x)])) ct)))
	(define replace																		;;Replaces the occurences of variables in the conflict-list with the register-homes
		(lambda (allocations ct)
			(cond 
				[(null? allocations) ct]
				[else (replace (cdr allocations) (replace-helper (car allocations) ct))])))
	(define replace-helper
		(lambda (allocation ct)
			(map (lambda (ct-entry)
							(cond
								[(eq? (car allocation) (car ct-entry)) allocation]
								[else (cons (car ct-entry) (replace* (cdr ct-entry) allocation))])) ct)))
	(define replace*
		(lambda (conflicts allocation)
			(cond
				[(null? conflicts) '()]
				[(eq? (car conflicts) (car allocation)) (cons (cadr allocation) (replace* (cdr conflicts) allocation))]
				[else (cons (car conflicts) (replace* (cdr conflicts) allocation))])))			
	(define k (length registers))
  (define low-degree?
    (lambda (var ct)
      (< (length (cdr (assq var ct))) k)))
(define num-conflicts
	(lambda (var ct)
		(length (cdr (assq var ct)))))
(define pick-min																										;;Picks a node with least number of conflicts like the min function
	(lambda (var degree var* ct)
		(cond
			[(null? var*) var]
			[(< degree (num-conflicts (car var*) ct)) (pick-min var degree (cdr var*) ct)]
			[else (let* ((node (car var*))
									(degree^ (num-conflicts node ct)))
									(pick-min node degree^ (cdr var*) ct))])))
(define find-homes
    (lambda (var* ct)
      (cond
				[(null? var*) '()]
				[(low-degree? (car var*) ct)
				 		(let* ((current-var (car var*))
								   (new-conflict-table (remove-occurence current-var ct))
									 (results (find-homes (cdr var*) new-conflict-table))
									 (updated-ct (replace results ct))
									 (conflict-entry (cdr (assq current-var updated-ct)))
									 (assign-register (car (difference registers conflict-entry))))
									 (cons (list current-var assign-register) results))]
				[else (let* ((current-var (pick-min (car var*) (num-conflicts (car var*) ct) (cdr var*) ct))
										(new-conflict-table (remove-occurence current-var ct))
										(results (find-homes (remq current-var var*) new-conflict-table))
										(updated-ct (replace results ct))
										(conflict-entry (cdr (assq current-var updated-ct)))
										(assign-register (car (difference registers conflict-entry))))
										(cons (list current-var assign-register) results))])))
	(define get-replacement
		(lambda (var entry)
					(list var (car (difference registers entry)))))
  (define Body
    (lambda (x)
      (match x
        [(locals (,uvar* ...) (register-conflict ,ct ,tail))
         (let ([home* (find-homes uvar* ct)])
           (let ([spill* (difference uvar* (map car home*))])
             (if (null? spill*)
                 `(locate (,home* ...) ,tail)
                 (error who "unable to assign registers to ~s" spill*))))]
        [,x (error who "invalid Body ~s" x)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,x (error who "invalid Program ~s" x)])))





;;------------------------------------------------------------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------------------------------------------------------------

;; This pass discards the Live* list included in each call.
;; This is done by the last match clause in the Tail statement

(define-who discard-call-live
  (define Tail
    (lambda (tail)
      (match tail
        [(begin ,ef* ... ,live*) `(begin ,ef* ... ,(Tail live*))]
	      [(if ,test ,[conseq],[alt])
	         `(if ,test ,conseq ,alt)]
	      [(,target ,live* ...)  `(,target)])))
  (define Body
    (lambda (bd)
      (match bd
        [(locate ([,uvar* ,reg*] ...) ,tail) `(locate ([,uvar* ,reg*] ...) ,(Tail tail))]
        [,bd (error who "invalid Body ~s" bd)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> bd*])] ...) ,[Body -> bd])
       `(letrec ([,label* (lambda () ,bd*)] ...) ,bd)]
      [,x (error who "invalid Program ~s" x)])))


;;------------------------------------------------------------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------------------------------------------------------------

;; Program	---->	(letrec ([label (lambda () Body)]*) Body)
;; Body	---->	(locate ([uvar reg]*) Tail)
;; Tail	---->	(Triv)
;;	     					|	(if Pred Tail Tail)
;;								|	(begin Effect* Tail)
;;Pred	---->	(true)
;;	|	(false)
;;	|	(relop Triv Triv)
;;	|	(if Pred Pred Pred)
;;	|	(begin Effect* Pred)
;;Effect	---->	(nop)
;;	|	(set! Var Triv)
;;	|	(set! Var (binop Triv Triv))
;;	|	(if Pred Effect Effect)
;;	|	(begin Effect* Effect)
;;Loc	---->	reg | fvar
;;Var	---->	uvar | Loc
;;Triv	---->	Var | int | label

;;This is the output produced by the discard-call-live
;;All occurences of uvar are replaced by the reg in the body, so now we have only regs in the instructions of the program

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
				[(begin ,effect* ... ,pred)
					 (let ((effects (replace-uvar effect* env)) (parsed-tail (Pred pred env)))
						`(begin ,@effects ,parsed-tail))]
				[(,relop ,triv1 ,triv2) `(,relop ,(Triv triv1 env) ,(Triv triv2 env))]
				[(if ,conde ,conseq ,alt) `(if ,(Pred conde env) ,(Pred conseq env) ,(Pred alt env))]
				)))
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


;;------------------------------------------------------------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------------------------------------------------------------

;;Responsible for exposing all frame vars..with a displacement operand

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


;;------------------------------------------------------------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------------------------------------------------------------

;;Program	---->	(letrec ([label (lambda () Tail)]*) Tail)
;;Tail	---->	(Triv)
;;	|	(if Pred Tail Tail)
;;	|	(begin Effect* Tail)
;;Pred	---->	(true)
;;	|	(false)
;;	|	(relop Triv Triv)
;;	|	(if Pred Pred Pred)
;;	|	(begin Effect* Pred)
;;Effect	---->	(nop)
;;	|	(set! Loc Triv)
;;	|	(set! Loc (binop Triv Triv))
;;	|	(if Pred Effect Effect)
;;	|	(begin Effect* Effect)
;;Loc	---->	reg | fvar
;;Triv	---->	Loc | int | label

;; This creates a new letrec binding for dealing with consequent and alternative parts of an if-expression so that the next pass can produce jumps more easily
;; We create a new binding for consequent and alternative at every stage we go on appending any new bindings created to a list and keep passing that list
;; Processing goes in a bottom up manner

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
	(define bind-function							;;Creates a letrec binding of form [(label (lambda() body))]
		(lambda (label statement)
			`((,label (lambda () ,statement)))))						
	(Program prog)))


;;------------------------------------------------------------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------------------------------------------------------------

;;Program	---->	(letrec ([label (lambda () Tail)]*) Tail)
;;Tail	---->	(Triv)
;;	|	(if (relop Triv Triv) (,label) (,label))
;;	|	(begin Effect* Tail)
;;Effect	---->	(set! Loc Triv)
;;	|	(set! Loc (binop Triv Triv))
;;Loc	---->	reg | disp-opnd
;;Triv	---->	Loc | int | label
;; In the tail function we also pass a list of labels in order, when we encounter an if expression in the tail body
;; we check the next label with conseq if same we introduce a not clause and jump to alt
;; else we check it with the alt label, if match is found we jump to conseq
;; else we have two options
;; (if cond (jump conseq))
;;(jump alt)
;or
;;
;;(if (not (< rax 3)) (jump alt))
;;(jump conseq) 

(define flatten-program
	(lambda (prog)
	(define build-exp
		(lambda (label* tail*)
			(match label*
				[() '()]
				[(,current ,rest* ...)
					(let ((current-exp (append (list current) (Tail rest* (car tail*)))))
						(append current-exp (build-exp rest* (cdr tail*))))])))
	(define Prog
		(lambda (x)
			(match x
				[(letrec ([,label* (lambda () ,tail*)] ...) ,tail)
					(let ((tail-exp (Tail label* tail)) (rest-of-exp (build-exp label* tail*)))
						(append '(code) tail-exp rest-of-exp))])))
	(define Tail
		(lambda (label* x)
			(match x
				[(if ,pred (,conseq) (,alt)) 
					(if (null? label*) `((if ,pred (jump ,conseq)) (jump ,alt))
						(let ((next-label (car label*)))
							(cond
								[(eq? next-label conseq) `((if (not ,pred) (jump ,alt)))]
								[(eq? next-label alt) `((if ,pred (jump ,conseq)))]
								[else `((if ,pred (jump ,conseq)) (jump ,alt))])))]
				[(begin ,effect* ...,tail) (append effect* (Tail label* tail))]		
				[(,triv) `((jump ,triv))])))
				(Prog prog)))


;;------------------------------------------------------------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------------------------------------------------------------

;;Converts an operator to instruction				
(define op->inst
  (lambda (op)
    (case op
      [(+) 'addq]
      [(-) 'subq]
      [(*) 'imulq]
      [(logand) 'andq]
      [(logor) 'orq]
      [(sra) 'sarq]
      [(=) 'je]
      [(<) 'jl]
      [(<=) 'jle]
      [(>) 'jg]
      [(>=) 'jge]
      [else (error who "unexpected binop ~s" op)])))


;;; Takes assembly instruction and returns the opposite of it

(define inst->inst^
				  (lambda (inst)
				    (case inst
				      [(je) 'jne]
				      [(jl) 'jge]
				      [(jle) 'jg]
				      [(jg) 'jle]
				      [(jge) 'jl]
				)))


;;; Same as assignment 2 except modified to handle conditional jumps

(define-who generate-x86-64
  (lambda (x)
    (define Program
      (lambda (x)
        (match x
          [(code ,st* ...)
           (emit-program (for-each Stmt st*))])))
    (define Stmt
      (lambda (x)
        (match x
          [(jump ,target) (emit-jump 'jmp target)]
          [(if (,op ,x ,y) (jump ,lbl)) (begin (emit 'cmpq y x) (emit-jump (op->inst op) lbl))]
          [(if (not (,op ,x ,y)) (jump ,lbl)) (begin (emit 'cmpq y x) (emit-jump (inst->inst^ (op->inst op)) lbl))]
          [(set! ,v1 (,op ,v1 ,v2)) (emit (op->inst op) v2 v1)]
          [(set! ,v1 ,v2) (guard (label? v2)) (emit 'leaq v2 v1)]
					[(set! ,v1 ,v2) (emit 'movq v2 v1)]
          [,label (emit-label label)])))
    (Program x)))
				
				



