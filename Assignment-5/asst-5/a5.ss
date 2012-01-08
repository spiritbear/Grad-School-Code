;;;Shiv Indap
;;;sindap
;;;Assignment 5
;;;P-523

;;; Andy Keep, Kent Dybvig
;;; P423/P523
;;; Spring 2009

;;; verify-scheme accept a single value and verifies that the value
;;; is a valid program in the grammar of the fourth assignment
;;;
;;; Grammar for verify-scheme (assignment 4):
;;;
;;; Program --> (letrec ((<label> (lambda () <Body>))*) <Body>)
;;; Body    --> (locals (<uvar>*) <Tail>)
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
;;; At this level in the compiler verify-scheme no longer checks machine
;;; constraints, as select-instructions should now perform instruction
;;; selection and correctly select which instruction to use based on the
;;; machine constraints.
;;;

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
        (when (and (integer? t) (exact? t))
          (unless (int64? t)
            (error who "integer out of 64-bit range ~s" t)))
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
          [(,predop ,[(Triv label* uvar*) -> x] ,[(Triv label* uvar*) -> y])
           (unless (memq predop '(= < <= > >=))
             (error who "invalid predicate operator ~s" predop))]
          [,pr (error who "invalid Pred ~s" pr)]))))
  (define Effect
    (lambda (label* uvar*)
      (lambda (ef)
        (match ef
          [(nop) (void)]
          [(set! ,[(Var uvar*) -> x] 
             (sra ,[(Triv label* uvar*) -> y] ,[(Triv label* uvar*) -> z]))
           (unless (uint6? z)
             (error who "invalid attempt to sra by ~s" z))]
          [(set! ,[(Var uvar*) -> x]
             (,binop ,[(Triv label* uvar*) -> y] ,[(Triv label* uvar*) -> z]))
           (unless (memq binop '(+ - logand logor * sra))
             (error who "invalid effect operator ~s" binop))]
          [(set! ,[(Var uvar*) -> x] ,[(Triv label* uvar*) -> y]) (void)]
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
           (unless (andmap (lambda (x) 
                             (or (frame-var? x) (register? x))) live-out*)
             (error who 
                    "live out list contains invalid variable ~s" live-out*))
           (when (integer? t)
             (error who "~s attempt to apply integer" `(,t)))]
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


;;---------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------

;; This section of the code simply prepares a live-set i.e the set of variables live before an assignment instruction is encountered
;; The update conflict table is responsible for updating the conflict table ct using side effects
;; if fv0 is current Lhs and {a.1 b.2 c.3} is live set we add the live-set to fv0's conflict
;; For an if-instruction encountered we take the union of both the conseqent and alternative parts and combine it
;; Processing of this code goes in a bottom up manner, since a frame can't conflict with a register only variables would be present in the conflict set

(define-who uncover-frame-conflict
	(define add-conflicts!
	    (trace-lambda conflicts(ct lhs live*)
	      (define add-conflict!
	        (lambda (var1 var2)
	          (let ([a (assq var1 ct)])
	            (set-cdr! a (if (eq? var1 var2) (cdr a)  (set-cons var2 (cdr a)))))))
	      (when (uvar? lhs)
	        (for-each
	          (lambda (live) (add-conflict! lhs live))
	          live*))
	      (for-each
	        (lambda (live) (when (and (uvar? live) (not (register? lhs))) (add-conflict! live lhs)))
	        live*)))
	(define Triv (lambda (x) (if (or (uvar? x) (frame-var? x)) `(,x) '())))
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
        (union live* (Pred test  c-live* a-live* ct))]
       [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
       [(set! ,lhs (,binop ,[Triv -> x-live*] ,[Triv -> y-live*])) 
					(begin
					(add-conflicts! ct lhs live*)
					(union x-live* y-live* (remq lhs live*)))]
       [(set! ,lhs ,[Triv -> var*]) 
					(begin
						(add-conflicts! ct lhs live*)
						(union var* (remq lhs live*)))
					]
       [,x (error who "invalid Effect list ~s" x)])))
			(define Pred
		    (lambda (x t-live* f-live* ct)
		      (match x
		        [(true) t-live*]
		        [(false) f-live*]
		        [(if ,test ,[c-live*] ,[a-live*])
		         (union t-live* f-live* (Pred test c-live* a-live* ct))]
		        [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
		        [(,predop ,[Triv -> x-live*] ,[Triv -> y-live*])
		         (remove-nulls (union x-live* y-live* t-live* f-live*))]
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
         (let ([ct (map (lambda (x) (cons x '())) uvar*)])
           (let ([live* (Tail tail ct)])
             `(locals (,uvar* ...) (frame-conflict ,ct ,tail))))]
        [,x (error who "invalid Body ~s" x)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,x (error who "invalid Program ~s" x)])))


;;---------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------

;; This pass wraps around a ulocals form around the body expression it doesnt process the Tail expressions at all

(define-who introduce-allocation-forms
  (define Body
		(lambda (x)
			(match x
				[(locals (,uvars* ...) (frame-conflict (,conflict* ...) ,body* ...)) 
					`(locals (,uvars* ...) (ulocals () (locate () (frame-conflict (,conflict* ...) ,body* ...))))])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,x (error who "invalid Program ~s" x)])))


;;---------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------

;;select-instructions does a signifcant rewrite of the code where necessary, the x86-64 architecture imposes certian limitations on the instructions
;;we are allowed to use, but the same cant be imposed on a programmer who writes programs, the select-instructions pass converts certian blocks of codes which may
;;be incompatible by x-86-64 instruction standards to a compatible form, the bulk of the rewrite has to be done when instructions of the form (set! x y) or
;;(set! x (op y z)) are encountered, another place where a rewrite would be neccesary is the place where we have relational operators
;;eg (set! fv0 rax) is converted to ((set! u.1 fv0) (set! u.1 rax)) because the mov expression only allows us to move a value to register location as 

(define-who select-instructions
  (define Effect
		(lambda (ef)
			(match ef
				[(nop) (values '(nop) '())]
				[(if ,[Pred -> pred plocals] ,[Effect -> conseq clocals] ,[Effect -> alt alocals])
					(values `(if ,pred ,conseq ,alt)
									 (append plocals clocals alocals))]
				[(begin ,[Effect -> ef* elocals*] ...) (values (make-begin `(,ef* ...)) (apply append elocals*))]
				[(set! ,lhs (,binop ,rhs1 ,rhs2))
					(cond
						[(eq? lhs rhs1) (complex-assignment-helper ef)]				;Handles expressions of the form (set! fv0 (+ fv0 rax))
						[(and (commutative? binop) (eq? lhs rhs2)) (complex-assignment-helper `(set! ,lhs (,binop ,rhs2 ,rhs1)))]
						[(eq? lhs rhs2) 
								(let* ((new-unspillable-variable (unique-name 't)) (stmt `(set! ,new-unspillable-variable (,binop ,new-unspillable-variable ,rhs2))))
									(let-values ([(complex-assignment complex-assignment-locals) (complex-assignment-helper stmt)])
										(values  `(begin (set! ,new-unspillable-variable ,rhs1)
																								,complex-assignment
																								(set! ,lhs ,new-unspillable-variable)) (append complex-assignment-locals `(,new-unspillable-variable)))))]
						[else
								(let-values ([(single-assignment single-assignment-locals) (simple-assignment `(set! ,lhs ,rhs1))]
														 [(complex-assignment complex-assignment-locals) (complex-assignment-helper `(set! ,lhs (,binop ,lhs ,rhs2)))])
														 (values `(begin ,single-assignment ,complex-assignment) (union single-assignment-locals complex-assignment-locals)))])] 
				[(set! ,x ,y) (simple-assignment ef)])))
	(define simple-assignment
		(lambda (exp)
			(match exp
				[(set! ,x ,y) (guard (or (uvar? x) (register? x))) (values `(set! ,x ,y) '())]
				[(set! ,x ,y) (guard (frame-var? x)) 
					(if (or (frame-var? y)  (label? y)) ;removed (int64? y)
							(let ((new-unspillable (unique-name 't)))
								(values `(begin (set! ,new-unspillable ,y) (set! ,x ,new-unspillable)) (list new-unspillable)))
							(values `(set! ,x ,y) '()))])))
	(define complex-assignment-helper
		(lambda (exp)
			(match exp
				[(set! ,lhs (,binop ,lhs ,rhs)) 
					(guard (memq binop '(+ - logand logor))) 
						(if (or (and (frame-var? lhs) (frame-var? rhs)) 
										(and (frame-var? lhs) (or (int64? rhs) (label? rhs))) 
										);(or (int64? rhs) (label? rhs))) 
											(let ((new-unspillable (unique-name 't)))
												(values `(begin (set! ,new-unspillable ,rhs) (set! ,lhs (,binop ,lhs ,new-unspillable))) (list new-unspillable))) 
											  (values exp '()))]
					[(set! ,lhs (,binop ,lhs ,rhs)) 
						(guard (and (eq? binop '*) (frame-var? lhs)))
						 	(let* ((new-unspillable-variable (unique-name 't)) (stmt `(set! ,new-unspillable-variable (,binop ,new-unspillable-variable ,rhs))))
								(let-values ([(complex-assignment complex-assignment-locals) (complex-assignment-helper stmt)])
									(values `(begin (set! ,new-unspillable-variable ,lhs)
																							,complex-assignment
																							(set! ,lhs ,new-unspillable-variable)) (append complex-assignment-locals `(,new-unspillable-variable)))))]
					[(set! ,lhs (,binop ,lhs ,rhs)) (guard (eq? binop 'sra)) (values exp '())]																		
					[(set! ,lhs (,binop ,lhs ,rhs)) (guard (or (uvar? lhs) (register? lhs))) (values exp '())])))
	(define relop-helper
		(lambda (x)
			(match x
				[(,relop ,triv1 ,triv2) (guard (or (uvar? triv1) (register? triv1))) (values `(,relop ,triv1 ,triv2) '())]
				[(,relop ,triv1 ,triv2) (guard (and (frame-var? triv1) (not (frame-var? triv2)))) (values `(,relop ,triv1 ,triv2) '())]
				[(,relop ,triv1 ,triv2) (guard (or (and (frame-var? triv1) (frame-var? triv2)) (and (int32? triv1) (int32? triv2)))) 
						(let ((new-unspillable (unique-name 't)))
							(values `(begin (set! ,new-unspillable ,triv1) (,relop ,new-unspillable ,triv2)) (list new-unspillable)))]
				[(,relop ,triv1 ,triv2) (guard (and (int32? triv1) (not (int32? triv2)))) (values `(,(operator^ relop) ,triv2 ,triv1) '())])))
  (define Pred 
		(lambda (x)
			(match x
				[(true) (values '(true) '())]
				[(false) (values '(false) '())]
				[(if ,[Pred -> pred plocals] ,[Pred -> conseq clocals] ,[Pred -> alt alocals])
					(values `(if ,pred ,conseq ,alt) (append plocals clocals alocals))]
				[(begin ,[Effect -> ef* elocals* ] ...,[Pred -> tail plocals]) 
					(values (make-begin `(,ef* ..., tail)) (apply append (cons plocals elocals*)))]
				[(,relop ,conseq ,alt) (relop-helper x)])))
  (define Tail 
		(lambda (x)
			(match x
				[(begin ,[Effect -> ef* elocal*] ... ,[Tail -> tail ulocal])
					(values  (make-begin `(,ef* ... ,tail))
									 (apply append ulocal elocal*))]
				[(if ,[Pred -> pred plocals] ,[Tail -> conseq clocals] ,[Tail -> alt alocals]) 
					(values `(if ,pred ,conseq ,alt) (append clocals alocals))]
				[(,loc* ...) (values `(,loc* ...) '())])))
  (define Body 
		(lambda (x)
    (match x
      [(locals (,local* ...) 
         (ulocals (,orig-ulocal* ...)
           (locate (,home* ...) (frame-conflict ,ct ,tail))))
       (let-values ([(tail ulocal*) (Tail tail)])
         `(locals (,local* ...)
            (ulocals (,orig-ulocal* ... ,ulocal* ...)
              (locate (,home* ...)
                (frame-conflict ,ct ,tail)))))]
      [(locate (,home* ...) ,tail) `(locate (,home* ...) ,tail)]
      [,x (error who "invalid Body ~s" x)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,x (error who "invalid Program ~s" x)])))

;;---------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------

;; This section of the code simply prepares a live-set i.e the set of variables live before an assignment instruction is encountered
;; The update conflict table is responsible for updating the conflict table ct using side effects
;; if x.1 is current Lhs and {a.1 r15 rax} is live set we add the live-set to x.1's conflict
;; For an if-instruction encountered we take the union of both the conseqent and alternative parts and combine it
;; Processing of this code goes in a bottom up manner

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
        [(locals (,local* ...) 
           (ulocals (,ulocal* ...)
             (locate (,home* ...)
               (frame-conflict ,fv-ct ,tail))))
         ;; setup the conflict table ct for storing conflicts
         (let ([ct (map (lambda (x) (cons x '())) (append local* ulocal*))])
           (let ([live* (Tail tail ct)])
             `(locals (,local* ...) 
                (ulocals (,ulocal* ...)
                  (locate (,home* ...)
                    (frame-conflict ,fv-ct
                      (register-conflict ,ct ,tail)))))))]
        [(locate (,home* ...) ,tail) `(locate (,home* ...) ,tail)]
        [,x (error who "invalid Body ~s" x)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,x (error who "invalid Program ~s" x)])))

;;---------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------

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
				[(<= degree (num-conflicts (car var*) ct)) (pick-min var degree (cdr var*) ct)]
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
									 	 (remaining-registers (difference registers conflict-entry)))
										 (if (null? remaining-registers) 
												results 
												(let ((assign-register (car remaining-registers)))
													(cons (list current-var assign-register) results))))]
					[else (let* ((current-var (pick-min (car var*) (num-conflicts (car var*) ct) (cdr var*) ct))
											(new-conflict-table (remove-occurence current-var ct))
											(results (find-homes (remq current-var var*) new-conflict-table))
											(updated-ct (replace results ct))
											(conflict-entry (cdr (assq current-var updated-ct)))
									 	  (remaining-registers (difference registers conflict-entry)))
										 (if (null? remaining-registers) 
												results 
												(let ((assign-register (car remaining-registers)))
													(cons (list current-var assign-register) results))))])))
		(define get-replacement
			(lambda (var entry)
						(list var (car (difference registers entry)))))
  (define Body
    (lambda (x)
      (match x
        [(locals (,local* ...) 
           (ulocals (,ulocal* ...)
             (locate (,frame-home* ...)
               (frame-conflict ,fv-ct
                 (register-conflict ,ct ,tail)))))
         ;; putting local* before ulocal* allows find-homes to choose the
         ;; first element of the list when all variables are high degree and
         ;; be guaranteed a spillable variable if one is left.  if find-homes
         ;; wants to be more clever about choosing a high-degree victim, it
         ;; will have to be told which variables are spillable.
         (let ([uvar* (append local* ulocal*)])
           (let ([home* (find-homes uvar* ct)])
             (let ([spill* (difference uvar* (map car home*))])
               (cond
                 [(null? spill*) `(locate (,frame-home* ... ,home* ...) ,tail)]
                 [(null? (intersection ulocal* spill*))
                  (let ([local* (difference local* spill*)])
                    `(locals (,local* ...)
                       (ulocals (,ulocal* ...)
                         (spills (,spill* ...)
                           (locate (,frame-home* ...)
                             (frame-conflict ,fv-ct ,tail))))))]
                 [else 
                  (error who "unspillable variables (~s) have been spilled"
                    (difference spill* local*))]))))]
        [(locate (,home* ...) ,tail) `(locate (,home* ...) ,tail)]
        [,x (error who "invalid Body ~s" x)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,x (error who "invalid Program ~s" x)])))


;;---------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------

;; Checks to see if all variables have got a home, and basically decides wether compiler should go on with allocation or stop

(define-who everybody-home?
  (define all-home?
    (lambda (body)
      (match body
        [(locals (,local* ...)
           (ulocals (,ulocal* ...)
             (spills (,spill* ...)
               (locate (,home* ...)
                 (frame-conflict ,ct ,tail))))) #f]
        [(locate (,home* ...) ,tail) #t]
        [,x (error who "invalid Body ~s" x)])))
  (lambda (x)
    (match x
       [(letrec ([,label* (lambda () ,body*)] ...) ,body)
        (andmap all-home? `(,body ,body* ...))]
       [,x (error who "invalid Program ~s" x)])))


;;------------------------------------------------------------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------------------------------------------------------------

(define-who assign-frame
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
	(define find-max
		(lambda (ls)
			(cond
				[(null? (cdr ls)) (car ls)]
				[else (max (car ls) (find-max (cdr ls)))])))
  (define find-homes 
		(lambda (var* ct home*)
      (cond
				[(null? var*) home*]
				[else (let* ((current-var (car var*)) 
										(new-conflict-table (remove-occurence current-var ct))
										(results (find-homes (remq current-var var*) new-conflict-table home*))
										(updated-ct (replace results ct))
										(conflict-entry (cdr (assq current-var updated-ct)))
										(max-val (find-max (map (lambda (z) (if (frame-var? z) (frame-var->index z) '-1)) conflict-entry)))
						 	  		(assigned-frame (index->frame-var (+ 1 max-val))))
									 	(cons (list current-var assigned-frame) results))])))
  (define Body
    (lambda (body)
      (match body
        [(locals (,local* ...)
           (ulocals (,ulocal* ...)
             (spills (,spill* ...)
               (locate (,home* ...)
                 (frame-conflict ,ct ,tail)))))
         (let ([home* (find-homes spill* ct home*)])
           `(locals (,local* ...)
              (ulocals (,ulocal* ...)
                (locate (,home* ...)
                  (frame-conflict ,ct ,tail)))))]
        [(locate (,home* ...) ,body) `(locate (,home* ...) ,body)]
        [,body (error who "invalid Body ~s" body)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,x (error who "invalid Program ~s" x)])))


;;------------------------------------------------------------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------------------------------------------------------------

;;Replaces all variables that have been allocated a frame with the appropriate frame variables

(define-who finalize-frame-locations
  (define Triv
			(lambda (env)
				(lambda (x)
					(if (or (integer? x) (label? x)) x ((Var env) x)))))
		(define Var
			(lambda (env)
				(lambda (x)
					(let ((entry-present (assq x env)))
						(if entry-present (cdr entry-present) x)))))   
  (define Pred
			(lambda (env)
				(lambda (x)
					(match x
						[(true) '(true)]
						[(false) '(false)]
						[(begin ,[(Effect env) -> effect*] ... ,[pred]) `(begin ,effect* ... ,pred)]
						[(,relop ,[(Triv env) -> triv1] ,[(Triv env) -> triv2]) `(,relop ,triv1 ,triv2)]
						[(if ,[test] ,[conseq] ,[alt]) `(if ,test ,conseq ,alt)]))))
  (define Effect
			(lambda (env)
				(lambda (effect)
					(match effect
						[(begin ,[effect]  ,[effect*] ...) `(begin ,effect ,effect* ...)]
						[(set! ,[(Var env) -> var] (,binop ,[(Triv env) -> triv1] ,[(Triv env) -> triv2])) `(set! ,var (,binop ,triv1 ,triv2))]
						[(set! ,[(Var env) -> var] ,[(Triv env) -> triv]) (if (eq? var triv) '(nop) `(set! ,var ,triv))]
						[(nop) '(nop)]
						[(if ,[(Pred env) -> test],[conseq] ,[alt]) `(if ,test ,conseq ,alt)]))))
  (define Tail
			(lambda (env)
				(lambda (x)
					(match x
						[(begin ,[(Effect env) -> effect* ]... ,[tail]) `(begin ,effect* ...,tail)]
						[(if ,[(Pred env) -> test ],[tail1] ,[tail2]) `(if ,test ,tail1 ,tail2)]
						[(,loc* ...) `(,loc* ...)]))))
  (define Body
    (lambda (bd)
      (match bd
        [(locals (,local* ...)
           (ulocals (,ulocal* ...)
             (locate ([,uvar* ,loc*] ...)
               (frame-conflict ,ct ,[(Tail (map cons uvar* loc*)) -> tail]))))
         `(locals (,local* ...)
            (ulocals (,ulocal* ...)
              (locate ([,uvar* ,loc*] ...)
                (frame-conflict ,ct ,tail))))]
        [(locate ([,uvar* ,loc*] ...) ,tail) 
         `(locate ([,uvar* ,loc*] ...) ,tail)]
        [,bd (error who "invalid Body ~s" bd)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> bd*])] ...) ,[Body -> bd])
       `(letrec ([,label* (lambda () ,bd*)] ...) ,bd)]
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
	(define Program
		(lambda (x)
			(match x
				[(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body]) 
						`(letrec ([,label* (lambda () ,body*)] ...),body)])))
	(define Body
		(lambda (bd)
			(match bd
				[(locate ([,uvar* ,loc*] ...) ,tail) ((Tail (map (lambda (x y) (cons x y)) uvar* loc*)) tail)])))
	(define Effect
		(lambda (env)
			(lambda (effect)
				(match effect
					[(begin ,[effect]  ,[effect*] ...) `(begin ,effect ,effect* ...)]
					[(set! ,[(Var env) -> var] (,binop ,[(Triv env) -> triv1] ,[(Triv env) -> triv2])) `(set! ,var (,binop ,triv1 ,triv2))]
					[(set! ,[(Var env) -> var] ,[(Triv env) -> triv]) `(set! ,var ,triv)]
					[(nop) '(nop)]
					[(if ,[(Pred env) -> test],[conseq] ,[alt]) `(if ,test ,conseq ,alt)]))))
	(define Pred
		(lambda (env)
			(lambda (x)
				(match x
					[(true) '(true)]
					[(false) '(false)]
					[(begin ,[(Effect env) -> effect*] ... ,[pred]) `(begin ,effect* ... ,pred)]
					[(,relop ,[(Triv env) -> triv1] ,[(Triv env) -> triv2]) `(,relop ,triv1 ,triv2)]
					[(if ,[test] ,[conseq] ,[alt]) `(if ,test ,conseq ,alt)]))))
	(define Triv
		(lambda (env)
			(lambda (x)
				(if (or (integer? x) (label? x)) x ((Var env) x)))))
	(define Var
		(lambda (env)
			(lambda (x)
				(if (uvar? x) (cdr (assq x env)) x))))			
	(define Loc
		(lambda (x)
			x))
	(define Tail
		(lambda (env)
			(lambda (x)
				(match x
					[(begin ,[(Effect env) -> effect* ]... ,[tail]) `(begin ,effect* ...,tail)]
					[(if ,[(Pred env) -> test ],[tail1] ,[tail2]) `(if ,test ,tail1 ,tail2)]
					[(,[(Triv env) -> triv]) `(,triv)]))))
(Program prog)))


;;------------------------------------------------------------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------------------------------------------------------------

;;Responsible for exposing all frame vars..with a displacement operand

(define-who expose-frame-var
  (define Triv
    (lambda (t)
      (if (frame-var? t)
          (make-disp-opnd 'rbp (ash (frame-var->index t) 3))
          t)))
  (define Pred
    (lambda (pr)
      (match pr
        [(true) '(true)]
        [(false) '(false)]
        [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
        [(begin ,[Effect -> ef*] ... ,[test])
         `(begin ,ef* ... ,test)]
        [(,predop ,[Triv -> tr1] ,[Triv -> tr2]) `(,predop ,tr1 ,tr2)]
        [,pr (error who "invalid Pred ~s" pr)])))
  (define Effect
    (lambda (st)
      (match st
        [(nop) '(nop)]
        [(set! ,[Triv -> var] (,binop ,[Triv -> t1] ,[Triv -> t2]))
         `(set! ,var (,binop ,t1 ,t2))]
        [(set! ,[Triv -> var] ,[Triv -> t])
         `(set! ,var ,t)]
        [(begin ,[ef] ,[ef*] ...) `(begin ,ef ,ef* ...)]
        [(if ,[Pred -> test] ,[conseq] ,[altern])
         `(if ,test ,conseq ,altern)]
        [,st (error who "invalid syntax for Effect ~s" st)])))
  (define Tail
    (lambda (tail)
      (match tail
        [(,[Triv -> t]) `(,t)]
        [(begin ,[Effect -> ef*] ... ,[tail])
         `(begin ,ef* ... ,tail)]
        [(if ,[Pred -> test] ,[conseq] ,[altern])
         `(if ,test ,conseq ,altern)]
        [,tail (error who "invalid syntax for Tail ~s" tail)])))
  (lambda (program)
    (match program
      [(letrec ([,label* (lambda () ,[Tail -> tail*])] ...) ,[Tail -> tail])
       `(letrec ([,label* (lambda () ,tail*)] ...) ,tail)]
      [,program (error who "invalid syntax for Program: ~s" program)])))

	
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

;; wether the operator is commutative or not
(define commutative?
	(lambda (x)
		(memq x '(+ * logand logor))))


(define operator^
	(lambda (op)
		(case op
			['> '<]
			['< '>]
			['<= '>=]
			['= '=]
			)))
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
				
				

