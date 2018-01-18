(module lang (lib "eopl.ss" "eopl")  
  
  (provide (all-defined-out))
  (require "lang.rkt")
  (require "hash.rkt")
  
  (define unif 'uninitialized)
  (define counter 0)
  (define increment
    (lambda () (set! counter (+ counter 1))))
  
  (define initialize-unification
    (lambda () (set! unif (make-h))))
  
  (define print-unif
    (lambda () (display unif)))
  
  (define unify-call
    (lambda (old-env args values)
      (let ((env (make-h)))
        (map (lambda(k v) 
               (cases term v
                 (a-variable (var) 
                             (h-set! env var counter)
                             (unify old-env k counter)
                             (increment))
                 (a-literal (num)
                            (unify old-env k num))
                 (else eopl:error 'invalid-clause-argument)))
             args
             values)
        env)))
  
  (define unify
    (lambda (env var val)
      (when (not (h-has-key? env var))
        (h-set! env var counter)
        (increment))
      (let* ((c (h-ref env var))
             (old-val (h-ref! unif c val)))
        (if (eq? val old-val)
            env
            (eopl:error 'already-unified)))))
  
  (define resolve-var
    (lambda (env var)
      (h-ref unif (h-ref env var))))
  
  (define make-init-env
    (lambda (vars)
      (let ((env (make-h)))
        (for-each 
         (lambda (var)
           (cases term var
             (a-variable (var) (h-set! env var counter) (increment))
             (else 1)))
         vars)
        env)))
  )