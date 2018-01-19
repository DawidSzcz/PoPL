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
  
  
  (define unif 'uninitialized)
  (define counter 0)
  (define increment
    (lambda () (set! counter (+ counter 1))))
  
  (define initialize-unification
    (lambda () (set! unif (make-h))))
  
  (define print-unif
    (lambda () (display unif)))
  
  (define extend-env
    (lambda (e v c)
      ;(display (list 'extend-env v c)) (newline)
      (h-set! e v c)
      ))
  
  
  (define unify-call
    (lambda (old-env args values)
      (let ((env (make-h)))
        (map (lambda(k v) 
               (cases term k
                 (a-variable (var-arg) 
                             (extend-env env var-arg counter)
                             (increment)
                             (cases term v
                               (a-variable (var-val) (unify old-env var-val (variable (h-ref env var-arg))))
                               (a-literal (num) (unify env var-arg (value num)))
                               (else eopl:error 'invalid-clause-argument)))
                 (a-literal (num)
                            (unify old-env (get-var v) num))
                 (else eopl:error 'invalid-call-variable)))
             args
             values)
        env)))
  
  (define unify
    (lambda (env var val)
      ;(display (list 'unifstart 'var var 'env env 'val val)) (newline) 
      (when (not (h-has-key? env var))
        (extend-env env var counter)
        (increment))
      (let* ((c (h-ref env var))
             (old-val (h-ref! unif c val)))
        ;(display (list 'unif  unif 'var var 'counter c 'old old-val 'val val)) (newline) 
        (if (eq? val old-val)
            env
            (eopl:error 'already-unified)))))
  
  (define resolve-var
    (lambda (env var)
      (resolve-unified-var (h-ref env var))))
  
  (define resolve-unified-var
    (lambda (c)
      (let ((resolved (h-ref unif c (lambda () (eopl:error 'variable-not-unified (list unif c))))))
        (cases unifval resolved
          (variable (var) (resolve-unified-var var))
          (value (val) val)))))
  
  (define make-init-env
    (lambda (vars)
      (let ((env (make-h)))
        (for-each 
         (lambda (var)
           (cases term var
             (a-variable (var) (extend-env env var counter) (increment))
             (else 1)))
         vars)
        env)))
  
  (define test-resolve
    (lambda (u env var expected)
      (set! unif u)
      (let ((resolved (resolve-var env var)))
            (set! unif (make-h))
        (display (list 'actual resolved)) (newline)
        (display (list 'expect expected)) (newline))))
  
  (define test-unify-call
    (lambda (u env args values expected)
      (set! unif u)
      (set! counter 0)
      (let ((fenv (unify-call env args values))
            (u unif))
        (set! unif (make-h))
        (set! counter 0)
        (display (list 'actual (list fenv u))) (newline)
        (display (list 'expect expected)) (newline))))
  
  ;(test-resolve 
  ;#hash((0 . #(struct:variable 2)) (2 . #(struct:value 3)))
   ;#hash(($y . 2) ($z . 0)) 
   ;'$z 
   ;2)
  
  (test-unify-call 
   (make-h) 
   (make-h) 
   (list (a-variable '$x)) 
   (list (a-literal 3)) 
   '( #hash(($x . 0)) #hash( (0 . #(struct:value 3)) )))
  )