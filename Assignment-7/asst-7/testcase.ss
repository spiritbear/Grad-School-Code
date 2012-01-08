(letrec ([ack$1 (lambda (m.1 n.2)
                      (locals (t.3)
                        (if (= m.1 0) (+ n.2 1)
													(if (= n.2 0) 
																(ack$1 (- m.1 1) 1)
																(begin
																	(set! t.3 (ack$1 m.1 (- n.2 1)))
																	(ack$1 (- m.1 1) t.3))))))])
   (locals () (ack$1 2 4)))
