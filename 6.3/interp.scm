(module interp (lib "eopl.ss" "eopl")
  
  ;; cps interpreter for the LETREC language, using the data structure
  ;; representation of continuations (Figure 5.3).
  
  ;; exercise: rewrite this using the procedural representation of
  ;; continuations (Figure 5.2).
  
  ;; exercise: rewrite this using a trampoline (page 159).
  
  (require "drscheme-init.scm")
  
  (require "lang.scm")
  (require "data-structures.scm")
  (require "store.scm")
  (require "environments.scm")
  
  (provide value-of-program value-of/k)
  
  ;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;
  
  ;; value-of-program : Program -> FinalAnswer
  ;; Page: 143 and 154
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)
      (cases program pgm
        (a-program (exp1)
                   (result-of/k exp1 (init-env) (end-cont))))))
  
  (define result-of/k
    (lambda (stat env cont)
      (cases statement stat
        (print-state (exp cont)
                     (begin
                       (write (expval->num (value-of exp env))) (newline)
                       (apply-com-cont cont (num-val 27))))
        (assign-state (var exp1)
                      (value-of/k exp1 env (set-com-cont var env cont)))
        (block-state (state states)
                     (begin 
                       (result-of state env (block-com-cont states env cont))))
        (if-state (exp state1 state2)
                  (value-of/k exp (if-com-cont state1 state2 env cont)))
        (while-state (exp state)
                     (value-of/k exp (while-com-cont exp state env cont)))
        (var-state (var vars state)
                   (let ((new-env (foldl 
                                   (lambda (nenv var) (extend-env var (newref 0) nenv)) 
                                   (extend-env var (newref 0) env)
                                   vars)))
                     (result-of/k state new-env cont)))
        ))))
  
(define apply-com-cont
  (lambda (cont val)
    (cases com-continuation cont
      (set-com-cont (var env cont)
                    (begin 
                      (setref! (apply-env env var) val)
                      (apply-cont cont (num-val 27))))
      (block-com-cont (states env cont)
                      (if (null? states)
                          (apply-com-cont cont (num-val 27))
                          (result-of/k (car states) env (block-com-cont (cdr states) env cont))))
      (if-com-cont (state1 state2 env saved-cont)
                   (if (expval->bool val)
                       (result-of/k state1 saved-env saved-cont)
                       (result-of/k state2 saved-env saved-cont)))
      (while-com-cont (exp state env saved-cont)
                      (if (expval->bool val)
                          (value-of/k exp (while-com-cont exp state env cont))
                          (apply-com-cont saved-cont (num-val 27))))     
      
      )))

  ;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page: 143--146, and 154
  (define value-of/k
    (lambda (exp env cont)
      (cases expression exp
        (const-exp (num) (apply-cont cont (num-val num)))
        (var-exp (var) (apply-cont cont (deref (apply-env env var))))
        (proc-exp (var body)
                  (apply-cont cont 
                              (proc-val (procedure var body env))))
        (letrec-exp (p-name b-var p-body letrec-body)
                    (value-of/k letrec-body
                                (extend-env-rec* p-name b-var p-body env)
                                cont))
        (zero?-exp (exp1)
                   (value-of/k exp1 env
                               (zero1-cont cont)))
        (let-exp (var exp1 body)
                 (value-of/k exp1 env
                             (let-exp-cont var body env cont)))
        (if-exp (exp1 exp2 exp3)
                (value-of/k exp1 env
                            (if-test-cont exp2 exp3 env cont)))
        (diff-exp (exp1 exp2)
                  (value-of/k exp1 env
                              (diff1-cont exp2 env cont)))        
        (call-exp (rator rand) 
                  (value-of/k rator env
                              (rator-cont rand env cont)))
        (begin-exp (exp1 exps)
                   (value-of/k exp1 env (block-cont exps env cont)))
        (assign-exp (var exp1)
                    (value-of/k exp1 env (set-cont var env cont)))
        )))
  
    
  (define apply-cont
    (lambda (cont val)
      (if (continuation? cont)
          (apply-sim-cont cont val)
          (apply-com-cont cont val))))
  ;; apply-cont : Cont * ExpVal -> FinalAnswer
  ;; Page: 148
  (define apply-sim-cont
    (lambda (cont val)
      (cases continuation cont
        (end-cont () 
                  (begin
                    (eopl:printf
                     "End of computation.~%")
                    val))
        ;; or (logged-print val)  ; if you use drscheme-init-cps.scm
        (zero1-cont (saved-cont)
                    (apply-cont saved-cont
                                (bool-val
                                 (zero? (expval->num val)))))
        (let-exp-cont (var body saved-env saved-cont)
                      (value-of/k body
                                  (extend-env var (newref val) saved-env) saved-cont))
        (if-test-cont (exp2 exp3 saved-env saved-cont)
                      (if (expval->bool val)
                          (value-of/k exp2 saved-env saved-cont)
                          (value-of/k exp3 saved-env saved-cont)))
        (diff1-cont (exp2 saved-env saved-cont)
                    (value-of/k exp2
                                saved-env (diff2-cont val saved-cont)))
        (diff2-cont (val1 saved-cont)
                    (let ((num1 (expval->num val1))
                          (num2 (expval->num val)))
                      (apply-cont saved-cont
                                  (num-val (- num1 num2)))))
        (rator-cont (rand saved-env saved-cont)
                    (value-of/k rand saved-env
                                (rand-cont val saved-cont)))
        (rand-cont (val1 saved-cont)
                   (let ((proc (expval->proc val1)))
                     (apply-procedure/k proc val saved-cont)))
        (block-cont (exps env cont)
                    (if (null? exps)
                        (apply-cont cont val)
                        (value-of/k (car exps) env (block-cont (cdr exps) env cont))))
        (set-cont (var env cont)
                  (begin 
                    (setref! (apply-env env var) val)
                    (apply-cont cont (num-val 27))))
        
        )))
  
  ;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
  ;; Page 152 and 155
  (define apply-procedure/k
    (lambda (proc1 arg cont)
      (cases proc proc1
        (procedure (var body saved-env)
                   (value-of/k body
                               (extend-env var (newref arg) saved-env)
                               cont)))))
  
  
  )




