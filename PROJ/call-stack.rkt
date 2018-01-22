(module call-stack racket 
  (require "predicates.rkt")
  (require "hash.rkt")
  (require "lang.rkt")
  
  (provide call-predicate call-next)
  
  (define cs 'uninitialized)
  
  (define init-cs
    (lambda ()
      (set! cs '())))
  
  (define call-predicate
    (lambda (id arg env)
      (let ((clauses (get-predicate id)))
        (set! cs (cons (list (cdr clauses) (environment (hash-copy (get-env env))) arg) cs))
        (car clauses))))
  
  (define call-next
    (lambda ()
      (display cs) (newline)
      (if (null? (caar cs))
          (let () 
            (set! cs (cdr cs))
            (call-next))
          (let ((cur-pred (car cs)))
            (set! cs (cons (list (cdar cur-pred) (cadr cur-pred) (caddr cur-pred)) (cdr cs)))
            (list (caar cur-pred) (cadr cur-pred) (caddr cur-pred))))))
  
  (define test-call-next
    (lambda (u expected)
      (set! cs u)
      (let ((pred (call-next))
            (n-cs cs ))
        (set! cs '())
        (display (list 'actual (list pred n-cs))) (newline)
        (display (list 'expect expected)) (newline))))  
  
  (test-call-next 
   '( (() '$x 'y) ((1 2 3 4) x y) )
   '((1 x y)(((2 3 4) x y))))
  
  (test-call-next 
   '( ((1) '$x 'y) ((1 2 3 4) '$x '$y) )
   '((1 '$x 'y) ((() '$x 'y)  ((1 2 3 4) '$x '$y))))
  
  (test-call-next 
   '( (() '$z 'r) (() '$x 'y) ((1 2 3 4) '$x '$y) )
   '((1 '$x '$y) (((2 3 4) '$x '$y))))
  
  (test-call-next 
   '(((((4) ())) x y))
   '((((4) ()) x y) ((() x y))))
  
  )