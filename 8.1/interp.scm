(module interp (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of/k)
  (provide trace-apply-procedure)

  (define trace-apply-procedure (make-parameter #f))

;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;


  (define-datatype continuation continuation?
    (end-cont)                          ; []
    (diff1-cont                       ; cont[(- [] (value-of e2 env))]
      (exp2 expression?)
      (env environment?)
      (cont continuation?))
    (diff2-cont                         ; cont[(- val1 [])]
      (val1 expval?)
      (cont continuation?))
    (unop-arg-cont
      (unop unary-op?)
      (cont continuation?))
    (if-test-cont
      (exp2 expression?)
      (exp3 expression?)
      (env environment?)
      (cont continuation?))
    (rator-cont            ; cont[(apply-proc [] (value-of rand env))]
      (rand expression?)
      (env environment?)
      (cont continuation?))
    (rand-cont                          ; cont[(apply-proc val1 [])]
      (val1 expval?)
      (cont continuation?))
    (try-cont
      (var symbol?)
      (handler-exp expression?)
      (env environment?)
      (cont continuation?))
    (raise1-cont
      (saved-cont continuation?))
    )

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (body)
          (value-of/k body (init-env) (list (end-cont)))))))

  ;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page: 173
  (define value-of/k
    (lambda (exp env cont)
      (cases expression exp
        
        (const-exp (num) (apply-cont cont (num-val num)))
        
        (const-list-exp (nums)
                        (apply-cont cont
                                    (list-val (map num-val nums))))
        
        (var-exp (var) (apply-cont cont (apply-env env var)))
        
        (diff-exp (exp1 exp2)
                  (value-of/k exp1 env
                              (cons (diff1-cont exp2 env (car cont)) (cdr cont))))
        
        (unop-exp (unop exp1)
                  (value-of/k exp1 env
                              (cons (unop-arg-cont unop (car cont)) (cdr cont))))
        
        (if-exp (exp1 exp2 exp3)
                (value-of/k exp1 env
                            (cons (if-test-cont exp2 exp3 env (car cont)) (cdr cont))))
        
        (proc-exp (var body)
                  (apply-cont cont
                              (proc-val
                               (procedure var body env))))
        
        (call-exp (rator rand)
                  (value-of/k rator env
                              (cons (rator-cont rand env (car cont)) (cdr cont))))
        
        ;; make let a macro, because I'm too lazy to add the extra
        ;; continuation
        (let-exp (var exp1 body)
                 (value-of/k
                  (call-exp (proc-exp var body) exp1)
                  env
                  cont))
        
        (letrec-exp (p-name b-var p-body letrec-body)
                    (value-of/k
                     letrec-body
                     (extend-env-rec p-name b-var p-body env)
                     cont))
        
        (try-exp (exp1 var handler-exp)
                 (value-of/k exp1 env
                             (cons (end-cont) (cons (try-cont var handler-exp env (car cont)) (cdr cont)))))
        
        (raise-exp (exp1)
                   (value-of/k exp1 env
                               (cons (raise1-cont (car cont)) (cdr cont)))))))

  ;; apply-cont : continuation * expval -> final-expval

  (define apply-cont
    (lambda (cont val)
      (if (null? cont)
          val
          (let ((cur-cont (car cont))
                (list-cont (cdr cont)))
            (cases continuation cur-cont
              (end-cont () (apply-cont list-cont val))
              (diff1-cont (exp2 saved-env saved-cont)
                          (value-of/k exp2 saved-env (cons (diff2-cont val saved-cont) list-cont)))
              (diff2-cont (val1 saved-cont)
                          (let ((n1 (expval->num val1))
                                (n2 (expval->num val)))
                            (apply-cont (cons saved-cont list-cont)
                                        (num-val (- n1 n2)))))
              (unop-arg-cont (unop saved-cont)
                             (apply-cont (cons saved-cont list-cont)
                                         (apply-unop unop val)))
              (if-test-cont (exp2 exp3 env saved-cont)
                            (if (expval->bool val)
                                (value-of/k exp2 env (cons saved-cont list-cont))
                                (value-of/k exp3 env (cons saved-cont list-cont))))
              (rator-cont (rand saved-env saved-cont)
                          (value-of/k rand saved-env
                                      (cons (rand-cont val saved-cont) list-cont)))
              (rand-cont (val1 saved-cont)
                         (let ((proc (expval->proc val1)))
                           (apply-procedure proc val (cons saved-cont list-cont))))
              ;; the body of the try finished normally-- don't evaluate the handler
              (try-cont (var handler-exp saved-env saved-cont)
                        (apply-cont (cons saved-cont list-cont) val))
              ;; val is the value of the argument to raise
              (raise1-cont (saved-cont)
                           ;; we put the short argument first to make the trace more readable.
                           (apply-handler val list-cont))
              )))))
      
  ;; apply-handler : ExpVal * Cont -> FinalAnswer
  (define apply-handler
    (lambda (val cont)
      (if (null? cont)
          (eopl:error 'apply-handler "uncaught exception!")
          (let ((cur-cont (car cont))
                (list-cont (cdr cont)))
            (cases continuation cur-cont
              ;; interesting cases
              (try-cont (var handler-exp saved-env saved-cont)
                        (value-of/k handler-exp
                                    (extend-env var val saved-env)
                                    (cons saved-cont list-cont)))
              
              (end-cont () 
                        (eopl:error 'apply-handler "Try expected!"))
              ;; otherwise, just look for the handler...
              (diff1-cont (exp2 saved-env saved-cont)
                          (eopl:error 'apply-handler "Try expected!"))
              (diff2-cont (val1 saved-cont)
                          (eopl:error 'apply-handler "Try expected!"))
              (if-test-cont (exp2 exp3 env saved-cont)
                            (eopl:error 'apply-handler "Try expected!"))
              (unop-arg-cont (unop saved-cont)
                             (eopl:error 'apply-handler "Try expected!"))
              (rator-cont (rand saved-env saved-cont)
                          (eopl:error 'apply-handler "Try expected!"))
              (rand-cont (val1 saved-cont)
                         (eopl:error 'apply-handler "Try expected!"))
              (raise1-cont (cont)
                           (eopl:error 'apply-handler "Try expected!"))
              )))))


  ;; apply-procedure : procedure * expval * cont -> final-expval

  (define apply-procedure
    (lambda (proc1 arg cont)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of/k body
            (extend-env var arg saved-env)
            cont)))))


  (define apply-unop
    (lambda (unop val)
      (cases unary-op unop
        (null?-unop ()
          (bool-val
            (null? (expval->list val))))
        (car-unop ()
          (car (expval->list val)))
        (cdr-unop ()
          (list-val (cdr (expval->list val))))
        (zero?-unop ()
          (bool-val
            (zero? (expval->num val)))))))


  ;; to get the detailed trace:
  ;; (trace value-of/k apply-cont apply-handler)
(value-of-program (scan&parse "try -(1, raise 44) catch (m) m"))
  )
