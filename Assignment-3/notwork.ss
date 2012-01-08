(letrec () 
  (locate () 
    (begin 
      (set! rax 0)
      (set! rbx 2)
      (if (= rax 0) 
          (if (= rbx 2) 
              (set! rax rbx)
              (set! rax 5)) 
          (set! rax 7))
      (r15))))


			(letrec ([if-test$6 (lambda ()
	                          (locate ([n.1 rdi] [x.2 rax] [y.3 rbx])
	                            (begin
	                              (set! x.2 1)
	                              (begin
	                                (set! y.3 1)
	                                (if (= n.1 0)
	                                    (set! x.2 (+ x.2 y.3))
	                                    (set! y.3 (+ y.3 x.2)))
	                                (set! x.2 n.1))
	                              (if (if (= n.1 y.3) (false) (true))
	                                  (set! n.1 (+ n.1 x.2))
	                                  (set! n.1 (+ n.1 y.3)))
	                              (set! x.2 n.1)
	                              (r15))))])
	      (locate ([n.1 rdi])
	        (begin
	          (set! n.1 1)
	          (if-test$6))))
	
	
						