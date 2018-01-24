(module call-stack racket 
  (require "predicates.rkt")
  (require "hash.rkt")
  (require "lang.rkt")
  (require "unification.rkt")
  
  (provide print-cs call-predicate call-next)
  
  (define cs '())
  
  
  (define print-cs
    (lambda ()
      (display 'call-stack) (newline)
      (display cs) (newline)
      (display "---------------------------------------------") (newline)))
  
  (define init-cs
    (lambda ()
      (set! cs '())))
  
  (define call-predicate
    (lambda (id args env)
      (let ((clauses (get-predicate id)))
        (set! cs (cons (list (cdr clauses) (environment (hash-copy (get-env env))) args (get-unif-copy)) cs))
        ;(display cs) (newline)
        (car clauses))))
  
  (define call-next
    (lambda ()
      ;(display cs) (newline)
      (if (null? (caar cs))
          (let () 
            (set! cs (cdr cs))
            (call-next))
          (let ((cur-pred (car cs)))
            (set! cs (cons (list (cdar cur-pred) (cadr cur-pred) (caddr cur-pred) (cadddr cur-pred)) (cdr cs)))
            (set-unif (cadddr cur-pred))
            (list (caar cur-pred) (cadr cur-pred) (caddr cur-pred) )))))
  
  (define test-call-next
    (lambda (u expected)
      (set! cs u)
      (let ((pred (call-next))
            (n-cs cs ))
        (set! cs '())
        (display 'new-cl) (newline)
        (display (list 'actual pred)) (newline)
        (display (list 'expect (car expected))) (newline)  
        (display 'stack) (newline)
        (display (list 'actual n-cs)) (newline)
        (display (list 'expect (cadr expected))) (newline)
        (display 'unif) (newline)
        (display (list 'actual unif)) (newline)
        (display (list 'expect (caddr expected))) (newline)
        (display "---------------------------------------------") (newline))))  
  
  (when tests
    
    (test-call-next 
     '(((((#(struct:a-literal 4)) ()))
       x
       y
       z))
     '((((#(struct:a-literal 4)) ()) x y) ((() x y z)) z))
    
  (test-call-next 
   '( (() x y p) ((1 2 3 4) x y s) )
   '((1 x y) (((2 3 4) x y s)) s))
  
  (test-call-next 
   '( ((1) x y s) ((1 2 3 4) x y z) )
   '((1 x y) ((() x y s)  ((1 2 3 4) x y z)) s))
  
  (test-call-next 
   '( (() z r z) (() x y z) ((1 2 3 4) x y z) )
   '((1 x y) (((2 3 4) x y z)) z))
  
  (test-call-next 
   '(((((4) ())) x y z))
   '((((4) ()) x y) ((() x y z)) z))
  
  ))