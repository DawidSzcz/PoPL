(module environments (lib "eopl.ss" "eopl") 
  
  ;; builds environment interface, using data structures defined in
  ;; data-structures.scm. 

  (require "data-structures.scm")

  (provide init-env empty-env extend-env apply-env extend-env-rec extend-env-rec*)
  
  (define (foldr f init seq) 
  (if (null? seq) 
      init 
      (f (car seq) 
         (foldr f init (cdr seq)))))

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;
  
  ;; init-env : () -> Env
  ;; usage: (init-env) = [i=1, v=5, x=10]
  ;; (init-env) builds an environment in which i is bound to the
  ;; expressed value 1, v is bound to the expressed value 5, and x is
  ;; bound to the expressed value 10.
  ;; Page: 69

  (define init-env 
    (lambda ()
      (extend-env 
       'i (num-val 1)
       (extend-env
        'v (num-val 5)
        (extend-env
         'x (num-val 10)
         (empty-env))))))
  
  (define empty-env
    (lambda () (lambda (search-var) (eopl:error 'apply-env "No binding for ~s" search-var))))
  (define extend-env
    (lambda (var val env) 
      (lambda (search-var) 
        (if (eqv? search-var var)
            val
            (env search-var)))))
  ; I changed a bit function from the book.
  ; I stayed with procedural type of environment but with the use of vector I inject outer env inside the procedure.
  (define extend-env-rec 
    (lambda (p-name b-var body saved-env)
      (let ((vec (make-vector 1)))
        (let ((new-env 
               (lambda (search-var) 
                 (if (eqv? search-var p-name)
                     (vector-ref vec 0)
                     (saved-env search-var)))))
          (vector-set! vec 0 (proc-val (procedure b-var body new-env)))
          new-env))))
  ; 
  (define extend-env-rec*
    (lambda (list env)
      (let ((vec (make-vector 1)))
        (let ((new-env (foldr 
                        (lambda (pr acc) 
                          (lambda (search-var)
                            (if (eqv? (car pr) search-var) 
                                (proc-val (procedure (cadr pr) (caddr pr) (vector-ref vec 0)))
                                (acc search-var))))
                        env
                        list)))
          (vector-set! vec 0 new-env)
          new-env))))
          
  
;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

  ;; Page: 86
(define apply-env
 (lambda (env search-var)
   (env search-var)))
  )
