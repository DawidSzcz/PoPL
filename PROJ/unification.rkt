(module lang (lib "eopl.ss" "eopl")  
  
  (provide (all-defined-out))
  (require "lang.rkt")
  (require "hash.rkt")
  
  (define-datatype unifval unifval?
    (variable
     (value number?))
    (value
     (value number?))
    )
  
  
  ; environment id -> unfval
  ;unif: unifval->unifval
  (define unif 'uninitialized)
  (define counter 0)
  (define increment
    (lambda () (set! counter (+ counter 1))))
  
  (define initialize-unification
    (lambda () (set! unif (make-h))))
  
  (define print-unif
    (lambda ()
      (display "unification") (newline)
      (display unif) (newline)
      (display "---------------------------------------------") (newline)))
  
  (define get-unif-copy
    (lambda () (h-copy unif)))
  
  (define set-unif
    (lambda (u) (set! unif u)))
  
  (define print-counter
    (lambda () (display counter) (newline)))
  
  (define variable?
    (lambda (var)
      (cases unifval var
        (variable (v) #t)
        (else #f))))
  
  
  (define resolve-or-extend
    (lambda (opac-env var)
      (let ((env (get-env opac-env)))
        (if (h-has-key? env var)
            (resolve-deepest (h-ref env var))
            (extend-env opac-env var)))))
  
  ; environment, id -> unifval
  (define extend-env
    (lambda (e v)
      (h-set! (get-env e) v (variable counter)) (increment) (variable (- counter 1))))
  
  ; (a-variable; a-value) , environment -> unifvar
  (define term->unifval
    (lambda (t env)
      (cases term t
        (a-variable (var-val) (resolve-or-extend env var-val))
        (a-literal (num) (value num))
        (else eopl:error 'invalid-term-arg-or-val t))))
  
  ; environment, list term, list term -> environment
  (define unify-call
    (lambda (old-env args values)
      (unify-aux old-env (environment (make-h)) args values)))
  
  
  (define unify-aux
    (lambda (old-env env k v)
      (if (null? k)
          env
          (let* ((arg (term->unifval (car k) env))
                 (val (term->unifval (car v) old-env))
                 (result (if (variable? val)
                             (unif-set val arg old-env)
                             (unif-set arg val env))))
            ;(display 'arg)(display arg)(newline)
            ;(display 'val)(display val)(newline)
            ;(display 'env)(display env)(newline)
            (cases retype result
              (environment (e) (unify-aux old-env env (cdr k) (cdr v)))
              (fail (f) result))))))
  
  
  ; unifval, unifval, environmant -> retype
  (define unif-set
    (lambda (var val env)
      (cases unifval var
        (value (v) (if (equal? var val)
                       env
                       (fail "imposible to unify")))
        (variable (v) (let ((old-val (h-ref! unif var val)))
                        (if (equal? val old-val)
                            env
                            (fail "already unified")))))))    
  
  ; (environment, term, unifval) -> environment 
  (define unify
    (lambda (env t val)
      (let ((var (term->unifval t env)))
        (unif-set var val env))))
  
  (define resolve-var
    (lambda (env var)
      (resolve-unified-var (h-ref (get-env env) var))))
  
  (define resolve-unified-var
    (lambda (c)
      (let ((resolved (h-ref unif c (lambda () (fail "variable-not-unified")))))
        (cases unifval resolved
          (variable (var) (resolve-unified-var resolved))
          (value (val) val)))))
  
  (define resolve-deepest
    (lambda (c)
      (if (h-has-key? unif c)
          (resolve-deepest (h-ref unif c))
          c)))
  
  (define make-init-env
    (lambda (vars)
      (let ((env (environment (make-h))))
        (for-each 
         (lambda (var)
           (cases term var
             (a-variable (var) (extend-env env var))
             (else 1)))
         vars)
        env)))
  
  (define test-resolve
    (lambda (u env var expected)
      (set! unif u)
      (let ((resolved (resolve-var env var)))
        (set! unif (make-h))
        (display (list 'actual resolved)) (newline)
        (display (list 'expect expected)) (newline)
        (display "---------------------------------------------") (newline))))
  
  (define test-unify-call
    (lambda (u c old-env args values expected)
      (set! unif u)
      (set! counter c)
      (let ((fenv (unify-call old-env args values))
            (u unif))
        (set! unif (make-h))
        (set! counter 0)
        (cases retype fenv
          (fail (s) 
                (display 'fail) (newline)
                (display (list 'actual fenv)) (newline)
                (display (list 'expected expected)) (newline))
          (environment (x)
                       (display 'old-env) (newline)
                       (display (list 'actual (get-env old-env))) (newline)
                       (display (list 'expected (car expected))) (newline)
                       (display 'env) (newline)
                       (display (list 'actual (get-env fenv))) (newline)
                       (display (list 'expected (cadr expected))) (newline)
                       (display 'unif) (newline)
                       (display (list 'actual u)) (newline)
                       (display (list 'expected (caddr expected))) (newline)))
        (display "---------------------------------------------") (newline))))
  
  (define test-unify
    (lambda (u c env var val expected)
      (set! unif u)
      (set! counter c)
      (let ((fenv (unify env var val))
            (u unif))
        (set! unif (make-h))
        (set! counter 0)
        (cases retype fenv
          (fail (s) 
                (display 'fail) (newline)
                (display (list 'actual fenv)) (newline)
                (display (list 'expected expected)) (newline))
          (environment (x)
                       (display 'env) (newline)
                       (display (list 'actual (get-env env))) (newline)
                       (display (list 'expected (car expected))) (newline)
                       (display 'unif) (newline)
                       (display (list 'actual u)) (newline)
                       (display (list 'expected (cadr expected))) (newline)))
        (display "---------------------------------------------") (newline))))
  
  (define resolve-query 
    (lambda (opac-env)
      (let ((env (get-env opac-env))
            (result (make-h)))
        (for-each
         (lambda (key)
           (h-set! result key (resolve-var opac-env key)))
         (h-keys env))
        result)))
  
  (when tests
    (test-resolve (h (list (list (variable 3) (value 3)) 
                           (list (variable 2) (value 3)) 
                           (list (variable 1) (variable 0)) 
                           (list (variable 0) (variable 3)))) 
                  (environment (h (list (list '$y (variable 1)) (list '$z (variable 0)))))
                  '$y 
                  3)
    
    (test-unify
     (make-h)
     0
     (environment (make-h)) 
     (a-variable '$x)
     (value 3)
     (list (h (list (list '$x (variable 0)))) (h (list (list (variable 0) (value 3))) )))   

    (test-unify-call 
     (h (list(list (variable 0) (variable 2))
       (list (variable 1)(value 2))
       (list (variable 2)(value 1))
       (list (variable 3)(value 2))
       (list (variable 4)(value 2))
       (list (variable 5)(value 1))
       (list (variable 6)(value 1))
       (list (variable 7)(value 2))
       (list (variable 8)(value 1))
       (list (variable 9)(value 1))
       (list (variable 10)(value 1))
       (list (variable 11)(value 2))
       (list (variable 12)(value 2))))
     13
     (environment (h (list (list '$X (variable 7))
         (list '$P (variable 9))
         (list '$Y (variable 8))
         (list '$R (variable 10))
         (list '$Y1 (variable 11))
         (list '$P1 (variable 12)))))
     (list (a-variable '$X) (a-variable '$X) (a-variable '$Z) (a-variable '$Z))
     (list (a-variable '$X) (a-variable '$Y1) (a-variable '$P1) (a-variable '$R))
     1)
    
    
    (test-unify-call 
     (h (list(list (variable 3) (value 2)) (list (variable 1) (value 3)) (list (variable 0) (variable 2))))
     4
     (environment (h (list (list '$y (variable 1)) (list '$a (variable 3)) (list '$z (variable 2)))))
     (list (a-variable '$y) (a-variable '$z))
     (list (a-variable '$a) (a-variable '$r))
     (list (h (list (list '$y (variable 1)) (list '$a (variable 3)) (list '$z (variable 2)) (list '$r (variable 6))))
           (h (list (list '$y (variable 4)) (list '$z (variable 5)))) 
           (h (list(list (variable 3) (value 2)) (list (variable 1) (value 3)) (list (variable 0) (variable 2)) (list (variable 4) (value 2)) (list (variable 6) (variable 5))))
           ))
    
    (test-unify-call 
     (make-h)
     0
     (environment (make-h)) 
     (list (a-literal 3) (a-variable '$x))
     (list (a-literal 3) (a-literal 3)) 
     (list (make-h) (h (list (list '$x (variable 0)))) (h (list (list (variable 0) (value 3))) )))
    
    
    (test-unify-call 
     (make-h)
     1
     (environment (make-h))
     (list (a-variable '$x)) 
     (list (a-literal 3)) 
     (list (make-h) (h (list (list '$x (variable 1)))) (h (list (list (variable 1) (value 3))) )))
    
    (test-unify-call 
     (make-h)
     1
     (environment (h (list(list '$x (variable 0)))))
     (list (a-literal 5)) 
     (list (a-variable '$x)) 
     (list (h (list (list '$x (variable 0)))) (make-h) (h (list (list (variable 0) (value 5)))) ))
    
    (test-unify-call 
     (make-h)
     1
     (environment (h (list(list '$x (variable 0)))))
     (list (a-variable '$x)) 
     (list (a-variable '$x)) 
     (list (h (list (list '$x (variable 0)))) (h (list (list '$x (variable 1)))) (h (list (list (variable 0) (variable 1)))) ))    
    
    (test-unify-call 
     (make-h)
     0
     (environment (h (list (list '$x (variable 5)))))
     (list (a-literal 5)) 
     (list (a-literal 3)) 
     #(struct:fail imposible to unify))
    ))