;Shiv Indap
;sindap
;P-523

;This program will generate scheme code
(load "verify-scheme.ss")


;-----------------------------------------------------------------------------------------------------------------------------------------------------
;Expose Frame var just replaces the frame variable by displacement mode operand
;All I do is check if the var or triv is a frame variable and then I simply make a displacment-mode operand and return it
;The rest of the code is similar to verify scheme
;The rest of the patterns are simply returned as they are

(define expose-frame-var
	(lambda (input-program)
		(define map-label-to-def
			(lambda (label* body*)
				(map make-func label* body*)))
		(define make-func
			(lambda (label body)
				(let ((transformed-body (Tail body)))
					`(,label (lambda () ,transformed-body)))))
		(define Program
			(lambda (x)
				(match x
					[(letrec ([,label* (lambda () ,tail*)] ...) ,tail-p)
							(let ((output (map-label-to-def label* tail*)) (ev-tail (Tail tail-p)))
								`(letrec ,output ,ev-tail))])))
			(define Tail
				(lambda (tail)
				  (match tail
				    [(begin ,stmt* ... ,tail-p)
								(let ((defn (map Effect stmt*)) (ev-tail (map Tail tail-p)))
				           `(begin ,@defn ,ev-tail))]
						[,triv-a
									(Triv triv-a)])))
			(define Effect
					(lambda (ef)
						(match ef
							[(set! ,var-p (,binop ,triv-a ,triv-b))
								`(set! ,(Var var-p) (,binop ,(Triv triv-a) ,(Triv triv-b)))]
							[(set! ,var-p ,triv-p)
							  `(set! ,(Var var-p) ,(Triv triv-p))])))
		 (define Var
		    (lambda (var)
	      	(if (frame-var? var) 
							(make-disp-opnd 'rbp (* 8 (frame-var->index var)))
							var)))
		  (define Triv
		    (lambda (t)
		      (if (frame-var? t)
							(make-disp-opnd 'rbp (* 8 (frame-var->index t)))
							t)))
		(Program input-program)))

;-----------------------------------------------------------------------------------------------------------------------------------------------------
;Flatten Program will generate the code in the form of a list beginning with code
;All tail calls have jump appended to them all labels are displayed as they are
;all letrecs and lambdas are stripped off, 
;Program defines the entry point into the code which atches with a letrec and lambda
;the functions make-body and make-label-def basically return a chunk of code
;specifying label and its actual definition which can be thought of as analogous to a sub-routine
;
;e.g
;(f$1
;:
;(set! rax rbx)
;:) in the form of a list
;
;Tail and Body are very similar maybe there is a better way of doing it but I couldnt get it to run for all the test cases
;Hence I wrote it differently
;The code is very similar to verify-scheme except that I expect the input to be properly formatted without errors
;The last statement of the tail has a jump prepended to it as indicated by the pattern

(define flatten-program
	(lambda (input-program)
		(define make-body
			(lambda (label* body*)
				(let ((func (map make-label-def label* body*)))
					(apply append func))))
		(define make-label-def
			(lambda (label body)
				(let ((transformed-body (Body body)))
					`(,label ,@transformed-body))))
		(define Program
			(lambda (x)
				(match x
					[(letrec ([,label* (lambda () ,tail*)] ...) ,tail-p)
									(let ((tail (Tail tail-p)) (defn (make-body label* tail*)))
										`(code ,@tail ,@defn))])))		;We evaluate the tail first because that is how the next pass requires its input
			(define Tail
				(lambda (tail)
				  (match tail
				    [(begin ,stmt* ... ,tail-p)
								(let ((effect-list (map Effect stmt*)) (tail (Tail tail-p)))
									`(,@effect-list ,@tail))
								]
						[(,triv-a)
									(list `(jump ,triv-a))])))
		 (define Body
				(lambda (body)
				  (match body
				    [(begin ,stmt* ... ,tail-p)
								(let ((defn (map Effect stmt*)) (ev-tail (Body tail-p)))
				           `(,@defn,ev-tail))]
						[(,triv-a)
									`(jump ,triv-a)])))
			(define Effect
					(lambda (ef)
						(match ef
							[(set! ,var-p (,binop ,triv-a ,triv-b))
								`(set! ,(Var var-p) (,binop ,(Triv triv-a) ,(Triv triv-b)))]
							[(set! ,var-p ,triv-p)
							  `(set! ,(Var var-p) ,(Triv triv-p))])))
		 (define Var
		    (lambda (var)
	      	var))
		  (define Triv
		    (lambda (t)
		      t))
		(Program input-program)))


;-----------------------------------------------------------------------------------------------------------------------------------------------------
;This takes a list in the form of
;input-program -----> code statement+
;Statement simply pattern matches the statement and decides which label to output
;it uses the helper emit-label in helpers.ss
;Binop simply returns which operator is to be used

(define generatecode
	(lambda (input-program)
		(define Program
			(lambda (x)
				(match x
					[(code ,stmt ,stmt* ...)
						(Statement stmt)
						(if (not (null? stmt*)) (Program `(code ,stmt* ...)))])))
		(define Statement
			(lambda (stmt)
				(match stmt
					[(set! ,reg ,lbl) (guard (label? lbl)) (emit 'leaq lbl reg)]
					[(set! ,var1 (,op ,var1 ,opnd)) (emit (Binop op) opnd var1)]
					[(set! ,var ,opnd) (emit 'movq opnd var)]
					[(jump ,opnd) (emit-jump 'jmp opnd)]
					[,var (guard (label? var)) (emit-label var)]
				)))
				(define Binop
					(lambda (x)
						(match x
							[+ 'addq]
							[- 'subq]
							[* 'imulq]
							[sra 'sarq]
							[logand 'andq]
							[logor 'orq]
							[,other (error 'parse "Invalid Operator ~s" other)])))
		(Program input-program)))

;This code generates the assembly code
;emit-program is used from helpers.ss and essentially generates the boiler plate code
;generate code generates the assembly code for the test programs
		
(define generate-x86-64
	(lambda (input-program)
		(emit-program (generatecode input-program))))
;-----------------------------------------------------------------------------------------------------------------------------------------------------		
